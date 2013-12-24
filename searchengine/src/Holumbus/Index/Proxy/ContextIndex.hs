module Holumbus.Index.Proxy.ContextIndex where

import           Control.Parallel.Strategies

import           Data.Binary                  (Binary(..))
import           Data.Text.Binary             ()
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Holumbus.Common
import qualified Holumbus.Index.Index         as Ix

-- ----------------------------------------------------------------------------

newtype ContextIndex impl v
    = ContextIx (Map Context (impl v))
    deriving (Show)

type ContextIxCon impl v
    = ( Ix.Index impl
      , Ix.ICon impl v
      )

-- ----------------------------------------------------------------------------

instance (Binary (impl v), Binary v) => Binary (ContextIndex impl v) where
  put (ContextIx i) = put i
  get = get >>= return . ContextIx

-- ----------------------------------------------------------------------------

-- | Insert a new context.
--   Note: If context already exists this function does nothing.
insertContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
insertContext c (ContextIx m) = ContextIx $ M.insertWith (const id) c Ix.empty m

-- | Removes context including attached index from ContextIndex.
deleteContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
deleteContext c (ContextIx m) = ContextIx $ M.delete c m

-- XXX: creates context if not exists - is that what we want?
-- | Insert an element to one Context.
insertWithCx :: ContextIxCon impl v
             => Context -> Ix.IKey impl v -> Ix.IVal impl v
             -> ContextIndex impl v -> ContextIndex impl v
insertWithCx c w v (ContextIx m)
  = case M.lookup c m of
      (Just _) -> ContextIx $ M.adjust (Ix.insert w v) c m
      _        -> ContextIx $ M.insertWith (const id) c (Ix.insert w v Ix.empty) m

-- | Insert an element to a list of contexts.
insertWithCxs :: ContextIxCon impl v
              => [Context] -> Ix.IKey impl v -> Ix.IVal impl v
              -> ContextIndex impl v -> ContextIndex impl v
insertWithCxs cs w v i = foldr (\c ix -> insertWithCx c w v ix) i cs

-- | Insert an element to all contexts.
insert :: ContextIxCon impl v
       => Ix.IKey impl v -> Ix.IVal impl v
       -> ContextIndex impl v -> ContextIndex impl v
insert w v (ContextIx m) = ContextIx $ M.map (Ix.insert w v) m

-- | Empty ContextIndex.
empty :: ContextIndex i v
empty = ContextIx $ M.empty

search :: ContextIxCon i v
       => Ix.ISearchOp i v -> Ix.IKey i v -> ContextIndex i v
       -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
search op k (ContextIx m) = M.toList $ M.map (Ix.search op k) m

lookupRange :: ContextIxCon i v
       => Ix.IKey i v -> Ix.IKey i v -> ContextIndex i v
       -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
lookupRange k1 k2 (ContextIx m) = M.toList $ M.map (Ix.lookupRange k1 k2) m

searchWithCx :: ContextIxCon i v
             => Ix.ISearchOp i v -> Context -> Ix.IKey i v -> ContextIndex i v
             -> [(Ix.IKey i v, Ix.IVal i v)]
searchWithCx op c k (ContextIx m)
  = case M.lookup c m of
      (Just cm) -> Ix.search op k cm
      _         -> []

searchWithCxs :: ContextIxCon i v
              => Ix.ISearchOp i v -> [Context] -> Ix.IKey i v -> ContextIndex i v
              -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
-- non parallel
-- searchWithCxs op cs k ix = concat $ L.map (\c -> searchWithCx op c k ix) cs
-- parallel
-- XXX: since we do not evaluate anything here
-- we dont have any real parallelism right now
searchWithCxs op cs k ix = parMap rseq (\c -> (c, searchWithCx op c k ix)) cs

-- | search in different contexts with key already normalized in respect
--   to each context type
searchWithCxsNormalized :: (ContextIxCon i v)
                           => Ix.ISearchOp i v
                           -> [(Context, Ix.IKey i v)]
                           -> ContextIndex i v
                           -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
searchWithCxsNormalized op cks ix
  = parMap rseq (\(c,k) -> (c, searchWithCx op c k ix)) cks


-- | Contexts/keys of 'ContextIndex'.
contexts :: ContextIndex i v -> [Context]
contexts (ContextIx m) = M.keys m

-- | Check if the context exists.
hasContext :: ContextIxCon i v
       => Context -> ContextIndex i v -> Bool
hasContext c (ContextIx m) = M.member c m

-- | Map a function iver all values of the 'ContextIndex'.
map :: (i v -> j w) -> ContextIndex i v -> ContextIndex j w
map f (ContextIx m) = ContextIx $ M.map f m
