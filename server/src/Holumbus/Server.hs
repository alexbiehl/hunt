{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
--import           Network.Wai.Middleware.Static

--import           Control.Monad            (mzero)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Concurrent.MVar

import           Data.Map                 (Map,{- empty-})
import qualified Data.Map                 as M
import           Data.Text                (Text)
--import qualified Data.Text                as T
--import           Data.Aeson hiding        (json)

--import qualified Data.Text.Lazy.Encoding as TEL
--import qualified Data.Text.Lazy as TL


--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J

import qualified Holumbus.Server.Template       as Tmpl
import           Holumbus.Server.Common

import           Holumbus.Index.Common          ( Position, Context
                                              --  , Word, URI Description
                                                , Document(..)
                                                , RawResult, DocId(..)
                                                , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common          as Co
import           Holumbus.Index.Common.Occurences
--import Holumbus.Index.Common.Document
import           Holumbus.Index.Inverted.PrefixMem
--import           Holumbus.Index.Common.RawResult
import           Holumbus.Index.CompactDocuments

import           Holumbus.Query.Language.Grammar
--import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Result


-- which ops should an indexer support? maybe already in crawler?
-- uses functional dependencies
class (HolIndex i, HolDocuments d) => HolIndexer ix i d | ix -> i d, i d -> ix where
  -- insert a new document (and the corresponding words and occurrences) into the indexer
  newIndexer                :: i -> d -> ix
  index                     :: ix -> i
  docTable                  :: ix -> d
  insertDoc                 :: Document -> Words -> ix -> ix
  searchPrefixNoCase        :: ix -> Context -> String -> RawResult
  allWords                  :: ix -> Context -> RawResult


-- generic indexer - combination of an index and a doc table
data Indexer i d
  = Indexer
    { ixIndex    :: i
    , ixDocTable :: d
    }


-- type class for an indexer - combination of index and doctable
instance (HolIndex i, HolDocuments d) => HolIndexer (Indexer i d) i d where
  newIndexer          i d                       = Indexer i d
  index               (Indexer i _)             = i
  docTable            (Indexer _ d)             = d
  searchPrefixNoCase                            = Co.prefixNoCase . index
  allWords                                      = Co.allWords . index
  -- FIXME: insert the doc and words as into the index and the document table
  insertDoc      doc wrds ix                    = newIndexer newIndex newDocTable
    where
    (dId, newDocTable) = Co.insertDoc (docTable ix) doc
    -- insertDoc                     :: d -> Document -> (DocId, d)
    newIndex           = foldr (\(c, w, ps) -> Co.insertOccurrences c w (mkOccs dId ps)) (index ix) $ flattenWords wrds

    mkOccs :: DocId -> [Position] -> Occurrences
    mkOccs did pl = insertOccs did pl emptyOccurrences

    insertOccs :: DocId -> [Position] -> Occurrences -> Occurrences
    insertOccs docId ws os = foldr (insertOccurrence docId) os ws
    
    flattenWords :: Map t (Map t1 t2) -> [(t, t1, t2)]
    flattenWords = concat . map (\(c, wl) -> map (\(w, ps)-> (c, w, ps)) $ M.toList wl) . M.toList


(.::) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.::) = (.).(.)

-- do something with the index
withIndex' :: MonadIO m => MVar a -> (a -> IO b) -> m b
withIndex' im a = liftIO $ readMVar im >>= a

-- modify the index
modIndex_ :: MonadIO m => MVar a -> (a -> IO a) -> m ()
modIndex_ = liftIO .:: modifyMVar_

-- modify the index with return value
modIndex :: MonadIO m => MVar a -> (a -> IO (a,b)) -> m b
modIndex = liftIO .:: modifyMVar

-- the indexer
indexer :: Indexer Inverted Documents
indexer = Indexer emptyInverted emptyDocuments

queryConfig :: ProcessConfig
queryConfig = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQuery :: (HolIndex i, HolDocuments d) => i -> d -> Query -> Result
runQuery = processQuery queryConfig

-- server itself:
--
--  -> should get some kind of state from command line or config file
--     f.e: which index impl to use, which doc store, which persistent backend etc...
start :: IO ()
start = scotty 3000 $ do

  -- index
  ixM    <- liftIO $ newMVar indexer
  let withIx = withIndex' ixM :: MonadIO m => (Indexer Inverted Documents -> IO b) -> m b
  let modIx_ = modIndex_ ixM :: MonadIO m => (Indexer Inverted Documents -> IO (Indexer Inverted Documents)) -> m ()

  -- request / response logging
  middleware logStdoutDev

  get "/" $ html Tmpl.index

  -- text "should get simple text query as param"
  get "/search/:query" $ do
    query <- param "query"
    res   <- withIx $ \(Indexer ix dx) -> do
                          let hits = docHits $ runQuery ix dx (Word query)
                          return $ map (\(_,(DocInfo doc _,_)) -> doc) (Co.toListDocIdMap hits)
    json $ JsonSuccess res

  get "/completion/:query" $ do
    query <- param "query"
    res   <- withIx $ \(Indexer ix dx) -> do
                          let hits = wordHits $ runQuery ix dx (Word query)
                          return $ map (\ (c, (_, o)) -> (c, M.fold (\m r -> r + Co.sizeDocIdMap m) 0 o)) (M.toList hits)
    json $ JsonSuccess res



  -- list all indexed documents
--  get "/search/:context/:query" $ do
--    context <- param "context"
--    query   <- param "query"
--    res <- withIx $ \i ->
--            return . show . Co.resultByWord context $ searchPrefixNoCase i context query
--    json $ JsonSuccess res

  -- list all words
  get "/words/:context" $ do
    context <- param "context"
    res <- withIx $ \i ->
            -- simple Text response
            return . show $ allWords i context
    json $ JsonSuccess res

  -- add a document
  post "/document/add" $ do
    -- Raises an exception if parse is unsuccessful
    js <- jsonData
    case js of
      ApiDocument u d ws  -> do
        -- transform doc
        let doc = Document u d
        modIx_ $ \ix ->
          return $ insertDoc doc ws ix
        json (JsonSuccess "doc added" :: JsonResponse Text)

  notFound . redirect $ "/"
