{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Indexer.PersistentIndexer where

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M

import           Holumbus.Utility                  (catMaybesSet)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common                   hiding (delete)
import           Holumbus.Common.DocIdMap          (toDocIdSet)
import qualified Holumbus.Common.Document          as Doc

import qualified Holumbus.Index.Index              as Ix
import qualified Holumbus.Index.TextIndex          as TIx
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Indexer.Indexer
import           Database.Persist
import qualified Database.Persist                  as DB

-- ----------------------------------------------------------------------------

type TextIndexerCon i m = (TIx.TextIndex i Occurrences, PersistStore m)
type TextIndexerConVal i m val = ( TIx.TextIndex i Occurrences 
                                 , PersistStore m 
                                 , PersistMonadBackend m ~ PersistEntityBackend val
                                 , PersistEntity val 
                                 , Indexable val
                                 )

class PersistEntity x => Indexable x where
  toDocId :: Key x -> DocId
  

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: TextIndexerConVal i m val
       => val -> Words -> ContextIndex i Occurrences -> m (ContextIndex i Occurrences)
insert entity wrds ix
    = do 
      dbId <- DB.insert entity
      return $ TIx.addWords wrds (toDocId dbId) ix

{--
-- | Update elements
update :: TextIndexerCon i dt 
       => DocId -> Dt.DValue dt -> Words 
       -> ContextTextIndexer i dt -> ContextTextIndexer i dt
update docId doc' w ix
  = insert doc' w ix'
  where
  ix' = delete ix (IS.singleton docId)

-- | Modify elements
modify :: (TextIndexerCon i dt)
       => (Dt.DValue dt -> Dt.DValue dt)
       -> Words -> DocId -> ContextTextIndexer i dt -> ContextTextIndexer i dt
modify f wrds dId (ii,dt)
  = (newIndex,newDocTable)
  where
  newDocTable = Dt.adjust f dId dt
  newIndex    = TIx.addWords wrds dId ii


-- | Delete a set if documents by 'URI'.
deleteDocsByURI :: TextIndexerCon i dt 
                => Set URI -> ContextTextIndexer i dt -> ContextTextIndexer i dt
deleteDocsByURI us ixx@(_ix,dt)
    = delete ixx docIds
    where
    docIds = toDocIdSet . catMaybesSet . S.map (Dt.lookupByURI dt) $ us

-- | Delete a set of documents by 'DocId'.
delete :: TextIndexerCon i dt => ContextTextIndexer i dt -> DocIdSet -> ContextTextIndexer i dt
delete (ix,dt) dIds
  = (newIx, newDt)
    where
    newIx = CIx.map (Ix.batchDelete dIds) ix
    newDt = Dt.difference dIds            dt

-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (TextIndexerCon i dt)
                      => Description -> Words -> DocId 
                      -> ContextTextIndexer i dt -> ContextTextIndexer i dt
modifyWithDescription descr wrds dId (ii,dt)
  = (newIndex, newDocTable)
  where
  newDocTable = Dt.adjust mergeDescr dId dt
  newIndex    = TIx.addWords wrds dId ii
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr  = Doc.update (\d' -> d'{ desc = flip M.union (desc d') descr })
--}
-- ----------------------------------------------------------------------------
