{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad.Trans                (liftIO)
import qualified Data.Map                           as M
import           Data.Text                          (Text)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Holumbus.Common                    hiding (delete)
import qualified Holumbus.Common.DocIdMap           as DM
import qualified Holumbus.Index.Proxy.ContextIndex  as Ix
import qualified Holumbus.Indexer.PersistentIndexer as Ixx

-- ----------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

-- ----------------------------------------------------------------------------

-- | this should be a more generic implementation since
--   should look the same for every entity from
--   the same database backend.
instance Ixx.Indexable (PersonGeneric SqlBackend) where
  toDocId (Key (PersistInt64 id)) = fromIntegral id
  toDocId _                       = error "not supported"
  fromDocId docId = Key $ PersistInt64 $ fromIntegral docId

-- ----------------------------------------------------------------------------

wordList :: WordList
wordList = M.fromList $ [("word", [1,5,10])]

wrds :: Word -> Words
wrds w = M.fromList $ [(w, wordList)]

-- ----------------------------------------------------------------------------

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    -- NOTE: everything just prototypes - a real impl would use the interpreter
    --
    -- create empty index
    ix <- Ixx.empty
    -- add person to index and "doctable" with persistent backend
    newIx <- Ixx.insert (Person "Chris" Nothing) (wrds "test") ix
    -- search in index with element
    let res@([(cx, [(wrd, docIdMap)])]) = Ix.lookup PrefixNoCase (Nothing, Just "word") newIx
    liftIO . print $ "ix result: " ++ (show res)
    liftIO . print $ "documents: " ++ (show docIdMap)

    let dbIds = (map (Ixx.fromDocId) $ DM.keys docIdMap) :: [Key Person]
    entities <- mapM get dbIds

    liftIO . print $ "db enttities found" ++ (show entities)

    return ()
    -- | regular persistent code
    {--
    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing
    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId
    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])
    john <- get johnId
    liftIO $ print (john :: Maybe Person)
    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]
    --}
