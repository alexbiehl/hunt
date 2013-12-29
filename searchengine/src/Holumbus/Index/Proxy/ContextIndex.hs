{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Holumbus.Index.Proxy.ContextIndex where

import           Control.Parallel.Strategies

import           Data.Binary                  (Binary(..))
import           Data.Text.Binary             ()
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Holumbus.Common
import qualified Holumbus.Index.Index         as Ix
import           Data.Text                    (Text)

-- ----------------------------------------------------------------------------

-- | ContextIndex stores different kinds of indexes
--   and hides their implementation. The API enforces
--   usage of a Text key.
newtype ContextIndex v = ContextIx 
    { contextIx :: Map Context (Ix.IndexImpl v) }
    deriving (Show)

instance (Binary (impl v), Binary v) => Binary (ContextIndex v) where
  put (ContextIx i) = put i
  get = get >>= return . ContextIx

-- ----------------------------------------------------------------------------

-- | Insert a new context.
--   Note: If context already exists this function does nothing.
insertContext :: Ix.IndexImplCon ix v  
              => Context -> ix v -> ContextIndex v -> ContextIndex v
insertContext c ix (ContextIx m) = ContextIx $ M.insertWith (const id) c (Ix.mkIndex ix) m

-- | Removes context including attached index from ContextIndex.
deleteContext :: Context -> ContextIndex v -> ContextIndex v
deleteContext c (ContextIx m) = ContextIx $ M.delete c m

-- | Insert an element to one Context.
insertWithCx :: Context -> Text -> v -> ContextIndex v -> ContextIndex v
insertWithCx c w v (ContextIx m)
  = case M.lookup c m of
      (Just _) -> ContextIx $ M.adjust adjust' c m
      _        -> error "context does not exist"
  where
  adjust' (Ix.IndexImpl ix) = Ix.mkIndex $ Ix.insert w v ix

-- | Insert an element to one Context.
deleteWithCx :: Context -> DocIdSet -> ContextIndex v -> ContextIndex v
deleteWithCx c dIds (ContextIx m)
  = case M.lookup c m of
      (Just _) -> ContextIx $ M.adjust adjust' c m
      _        -> error "context does not exist"
  where
  adjust' (Ix.IndexImpl ix) = Ix.mkIndex $ Ix.batchDelete dIds ix


-- | Insert an element to a list of contexts.
insertWithCxs :: [Context] -> Text -> v -> ContextIndex v -> ContextIndex v
insertWithCxs cs w v i = foldr (\c ix -> insertWithCx c w v ix) i cs

-- | Insert an element to a list of contexts.
deleteWithCxs :: [Context] -> DocIdSet -> ContextIndex v -> ContextIndex v
deleteWithCxs cs dIds i = foldr (\c ix -> deleteWithCx c dIds ix) i cs

-- | Insert an element to one Context.
deleteWithAllCxs :: DocIdSet -> ContextIndex v -> ContextIndex v
deleteWithAllCxs dIds (ContextIx m)
  = ContextIx $ M.map adjust' m
  where
  adjust' (Ix.IndexImpl ix) = Ix.mkIndex $ Ix.batchDelete dIds ix




-- | Empty ContextIndex.
empty :: ContextIndex v
empty = ContextIx $ M.empty

search :: TextSearchOp -> Text -> ContextIndex v -> [(Context, [(Text, v)])]
search op k (ContextIx m) 
  = M.toList $ M.map search' m
  where
  search' (Ix.IndexImpl ix) = Ix.search op k ix

lookupRange :: Text -> Text -> ContextIndex v
            -> [(Context, [(Text, v)])]
lookupRange k1 k2 (ContextIx m) 
  = M.toList $ M.map range' m
  where
  range' (Ix.IndexImpl ix) = Ix.lookupRange k1 k2 ix

searchWithCx :: TextSearchOp 
             -> Context -> Text -> ContextIndex v -> [(Text, v)]
searchWithCx op c k (ContextIx m)
  = case M.lookup c m of
      (Just (Ix.IndexImpl cm)) -> Ix.search op k cm
      _                        -> []
  

-- | XXX we actually do not have any parallelism here at the moment
--   because everything is evalutated lazy!
searchWithCxs :: TextSearchOp -> [Context] -> Text -> ContextIndex v
              -> [(Context, [(Text, v)])]
searchWithCxs op cs k (ContextIx m) 
  = parMap rseq search' cs
  where
  search' c = case M.lookup c m of
      (Just (Ix.IndexImpl cm)) -> (c, Ix.search op k cm)
      _                        -> (c, [])


-- | search in different contexts with key already normalized in respect
--   to each context type
searchWithCxsNormalized :: TextSearchOp
                        -> [(Context, Text)]
                        -> ContextIndex v
                        -> [(Context, [(Text, v)])]
searchWithCxsNormalized op cks (ContextIx m)
  = parMap rseq search' cks
  where
  search' (c, k) = case M.lookup c m of
      (Just (Ix.IndexImpl cm)) -> (c, Ix.search op k cm)
      _                        -> (c, [])

-- | Contexts/keys of 'ContextIndex'.
contexts :: ContextIndex v -> [Context]
contexts (ContextIx m) = M.keys m

-- | Check if the context exists.
hasContext :: Context -> ContextIndex v -> Bool
hasContext c (ContextIx m) = M.member c m

-- | Map a function iver all values of the 'ContextIndex'.
map :: Ix.IndexImplCon i v
    => (i v -> i v) -> ContextIndex v -> ContextIndex v
--map f (ContextIx m) = ContextIx $ M.map (\(Ix.IndexImpl ix) -> Ix.mkIndex $ f ix) m
map = undefined


