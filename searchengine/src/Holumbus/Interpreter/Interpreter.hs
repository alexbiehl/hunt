{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Holumbus.Interpreter.Interpreter where

import           Control.Concurrent.MVar
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Set                          (Set)
import           Data.Text                         (Text)
import qualified Data.Text                         as T


import           Holumbus.Common.BasicTypes
import           Holumbus.Common.ApiDocument       as ApiDoc
import           Holumbus.Common.Occurrences       (Occurrences)
import           Holumbus.Common.Document          (Document, unwrap)
import qualified Holumbus.Common.DocIdMap          as DM

import           Holumbus.Analyzer.Analyzer

import           Holumbus.Indexer.TextIndexer      (ContextTextIndexer)
import qualified Holumbus.Indexer.TextIndexer      as Ixx

import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result             as QRes

import qualified Holumbus.DocTable.DocTable        as Dt
import           Holumbus.DocTable.HashedDocuments as HDt

import           Holumbus.Interpreter.Command
-- ----------------------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- Env, Index, ...

-- ----------------------------------------------------------------------------
--
-- the indexer used in the interpreter
-- this should be a generic interpreter in the end
-- but right now its okay to have the indexer
-- replaceable by a type declaration

type IpIndexer = ContextTextIndexer InvertedIndex (Documents Document)

emptyIndexer    :: IpIndexer
emptyIndexer    = (CIx.empty, HDt.empty)

-- ----------------------------------------------------------------------------

{-
data Options = Options
    { opSplitter :: ApiDocument -> (Document, Words)
    }

emptyOptions :: Options
emptyOptions = Options
    { opSplitter = toDocAndWords
    }
-}

data Options = Options

emptyOptions :: Options
emptyOptions = Options

-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)

data Env = Env
    { evIndexer :: MVar IpIndexer
    , evOptions :: Options
    }

initEnv :: IpIndexer -> Options -> IO Env
initEnv ixx opt
    = do ixref <- newMVar ixx
         return $ Env ixref opt

-- ----------------------------------------------------------------------------
-- the command evaluation monad
-- ----------------------------------------------------------------------------
newtype CMT m a = CMT { runCMT :: ReaderT Env (ErrorT CmdError m) a }
  deriving (Monad, MonadIO, Functor, MonadReader Env, MonadError CmdError)

instance MonadTrans CMT where
  lift = CMT . lift . lift

type CM = CMT IO

-- ----------------------------------------------------------------------------

runCM :: CMT m a -> Env -> m (Either CmdError a)
runCM env = runErrorT . runReaderT (runCMT $ env)

runCmd :: Env -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: CM IpIndexer
askIx
    = do ref <- asks evIndexer
         liftIO $ readMVar ref

-- FIXME: io exception-safe?
modIx :: (IpIndexer -> CM (IpIndexer, a)) -> CM a
modIx f
    = do ref <- asks evIndexer
         ix <- liftIO $ takeMVar ref
         (i',a) <- f ix `catchError` putBack ref ix
         liftIO $ putMVar ref i'
         return a
    where
    putBack ref i e = do
        liftIO $ putMVar ref i
        throwError e

modIx_ :: (IpIndexer -> CM IpIndexer) -> CM ()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

withIx :: (IpIndexer -> CM a) -> CM a
withIx f
    = askIx >>= f

askOpts :: CM Options
askOpts
    = asks evOptions

throwResError :: Int -> Text -> CM a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: String -> CM a
throwNYI c = throwResError 501 $ "command not yet implemented: " `T.append` (T.pack c)

-- ----------------------------------------------------------------------------

execCmd :: Command -> CM CmdResult
execCmd (Search q)
    = withIx $ case q of
        Right qry -> execSearch $ qry
        Left  str ->
          case parseQuery (T.unpack str) of
            Right qry -> execSearch $ qry
            Left  err -> const $ throwResError 500 err

execCmd (Sequence cs)
    = execSequence cs

execCmd NOOP
    = return ResOK  -- keep alive test

execCmd (Insert doc opts)
    = modIx $ execInsert doc opts

execCmd (Delete uri)
    = modIx $ execDelete uri

execCmd c
    = throwNYI $ show c

-- ----------------------------------------------------------------------------

execSearch :: Query -> IpIndexer -> CM CmdResult
execSearch q (ix, dt) = do
    res <- runQueryM ix dt q
    return $ ResSearch $ map (\(_, (DocInfo d _, _)) -> unwrap d) . DM.toList . docHits $ res


execSequence :: [Command] -> CM CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs


execInsert :: ApiDocument -> InsertOption -> IpIndexer -> CM (IpIndexer, CmdResult)
execInsert doc op ixx = do
    --split <- asks (opSplitter . evOptions)
    let split = toDocAndWords
    let (docs, ws) = split doc
    let ix'        = Ixx.insert docs ws ixx
    case op of
        New     -> return (ix', ResOK) -- TODO: not the real deal yet
        x       -> throwNYI $ show x


execDelete :: Set URI -> IpIndexer -> CM (IpIndexer, CmdResult)
execDelete d ix = do
    let ix' = Ixx.deleteDocsByURI d ix
    return (ix', ResOK)

-- ----------------------------------------------------------------------------

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: ContextIndex InvertedIndex Occurrences
                -> Documents Document
                -> Query
                -> CM (QRes.Result (Dt.DValue (Documents Document)))
runQueryM       = processQueryM queryConfig
