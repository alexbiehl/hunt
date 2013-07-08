module Holumbus.Index.Index
where

import           Control.Arrow
import           Data.Function                  (on)
import           Data.List                      (foldl', sortBy)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Set                       (Set)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

import qualified Holumbus.Data.PrefixTree       as PT

import           Holumbus.Index.Common          (Context, DocId, Occurrences,
                                                 Position, RawResult, Word,
                                                 emptyOccurrences,
                                                 mergeOccurrences,
                                                 resultByDocument, resultByWord,
                                                 singletonOccurrence,
                                                 sizeOccurrences, sizePos,
                                                 substractOccurrences, unionPos)
import qualified Holumbus.Index.Common.DocIdMap as DM
import           Holumbus.Index.Compression     as C

-- ----------------------------------------------------------------------------
--
-- external interface

sizeWords             :: Index -> Int
sizeWords             = _sizeWords

-- | Returns a list of all contexts avaliable in the index.
contexts              :: Index -> [Context]
contexts              = _contexts

-- | Returns the occurrences for every word. A potentially expensive operation.
allWords              :: Index -> Context -> RawResult
allWords              = _allWords

-- | Searches for words beginning with the prefix in a given context (case-sensitive).
prefixCase            :: Index -> Context -> Text -> RawResult
prefixCase            = _prefixCase

-- | Searches for words beginning with the prefix in a given context (case-insensitive).
prefixNoCase          :: Index -> Context -> Text -> RawResult
prefixNoCase          = _prefixNoCase

-- | Searches for and exact word in a given context (case-sensitive).
lookupCase            :: Index -> Context -> Text -> RawResult
lookupCase            = _lookupCase

-- | Searches for and exact word in a given context (case-insensitive).
lookupNoCase          :: Index -> Context -> Text -> RawResult
lookupNoCase          = _lookupNoCase

-- | Insert occurrences.
insertOccurrences     :: Context -> Word -> Occurrences -> Index -> Index
insertOccurrences     = \c w o i -> _insertOccurrences i c w o

-- | Delete occurrences.
deleteOccurrences     :: Context -> Word -> Occurrences -> Index -> Index
deleteOccurrences     = \c w o i -> _deleteOccurrences i c w o

-- | Insert a position for a single document.
insertPosition        :: Context -> Word -> DocId -> Position -> Index -> Index
insertPosition        = \c w d p i -> _insertPosition i c w d p
--insertPosition c w d p i      ,, insertOccurrences c w (singletonOccurrence d p) Index

-- | Delete a position for a single document.
deletePosition        :: Context -> Word -> DocId -> Position -> Index -> Index
deletePosition        = \c w d p i -> _deletePosition i c w d p
--deletePosition c w d p i      ,, deleteOccurrences c w (singletonOccurrence d p) Index

-- | Delete documents completely (all occurrences).
deleteDocsById        :: Set DocId -> Index -> Index
deleteDocsById        = \ds i -> _deleteDocsById i ds

-- | Merges two indexes.
--mergeIndexes          :: Index -> Index -> Index
--mergeIndexes i1 i2    = _mergeIndexes i1 i2

-- | Substract one index from another.
--substractIndexes      :: Index -> Index -> Index
--substractIndexes      = _substractIndexes

-- | Splitting an index by its contexts.
splitByContexts       :: Index -> Int -> [Index]
splitByContexts       = _splitByContexts

-- | Splitting an index by its documents.
splitByDocuments      :: Index -> Int -> [Index]
splitByDocuments      = _splitByDocuments

-- | Splitting an index by its words.
splitByWords          :: Index -> Int -> [Index]
splitByWords          = _splitByWords

-- | Update document id's (e.g. for renaming documents). If the function maps two different id's
-- to the same new id, the two sets of word positions will be merged if both old id's are present
-- in the occurrences for a word in a specific context.
updateDocIds          :: (Context -> Word -> DocId -> DocId) -> Index -> Index
updateDocIds          = \f i -> _updateDocIds i f

-- | Update document id's with a simple injective editing function.
updateDocIds'         :: (DocId -> DocId) -> Index -> Index
updateDocIds'         = \f i -> _updateDocIds' i f
--updateDocIds' f               ,, updateDocIds (const . const $ f)

-- Convert an Index to a list. Can be used for easy conversion between different index
-- implementations
toList                :: Index -> [(Context, Word, Occurrences)]
toList                = _toList

-- XXX: fromList is not in the index data type
-- Create an Index from a list. Can be used for easy conversion between different index
-- implementations. Needs an empty index as first argument
fromList              :: [(Context, Word, Occurrences)] -> Index
fromList              = newIndex . fromList'
--fromList e                    ,, foldl (\i (c,w,o) -> insertOccurrences c w o i) e

-- ----------------------------------------------------------------------------

data Index = Ix
    {
    -- | Returns the number of unique words in the index.
      _sizeWords                     :: Int

    -- | Returns a list of all contexts avaliable in the index.
    , _contexts                      :: [Context]

    -- | Returns the occurrences for every word. A potentially expensive operation.
    , _allWords                      :: Context -> RawResult

    -- | Searches for words beginning with the prefix in a given context (case-sensitive).
    , _prefixCase                    :: Context -> Text -> RawResult

    -- | Searches for words beginning with the prefix in a given context (case-insensitive).
    , _prefixNoCase                  :: Context -> Text -> RawResult

    -- | Searches for and exact word in a given context (case-sensitive).
    , _lookupCase                    :: Context -> Text -> RawResult

    -- | Searches for and exact word in a given context (case-insensitive).
    , _lookupNoCase                  :: Context -> Text -> RawResult

    -- | Insert occurrences.
    , _insertOccurrences             :: Context -> Word -> Occurrences -> Index

    -- | Delete occurrences.
    , _deleteOccurrences             :: Context -> Word -> Occurrences -> Index

    -- | Insert a position for a single document.
    , _insertPosition                :: Context -> Word -> DocId -> Position -> Index
    --insertPosition c w d p i      = insertOccurrences c w (singletonOccurrence d p) Index

    -- | Delete a position for a single document.
    , _deletePosition                :: Context -> Word -> DocId -> Position -> Index
    --deletePosition c w d p i      = deleteOccurrences c w (singletonOccurrence d p) Index

    -- | Delete documents completely (all occurrences).
    , _deleteDocsById                :: Set DocId -> Index

    -- | Merges two indexes.
    --, _mergeIndexes                  :: Index -> Index

    -- | Substract one index from another.
    --, _substractIndexes              :: Index -> Index

    -- | Splitting an index by its contexts.
    , _splitByContexts               :: Int -> [Index]

    -- | Splitting an index by its documents.
    , _splitByDocuments              :: Int -> [Index]

    -- | Splitting an index by its words.
    , _splitByWords                  :: Int -> [Index]

    -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    , _updateDocIds                  :: (Context -> Word -> DocId -> DocId) -> Index

    -- | Update document id's with a simple injective editing function.
    , _updateDocIds'                 :: (DocId -> DocId) -> Index
    --updateDocIds' f               = updateDocIds (const . const $ f)

    -- Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations

    , _toList                        :: [(Context, Word, Occurrences)]

    -- Create an Index from a list. Can be used for easy conversion between different index
    -- implementations. Needs an empty index as first argument

    --, _fromList                      :: [(Context, Word, Occurrences)] -> Index
    --fromList e                    = foldl (\i (c,w,o) -> insertOccurrences c w o i) e
    }


-- ----------------------------------------------------------------------------
--
-- PrefixMem implementation


newtype Inverted        = Inverted { indexParts :: Parts }
                          deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.

type Parts              = Map Context Part

-- | The index part is the real inverted index. Words are mapped to their occurrences.

type Part               = PT.PrefixTree CompressedOccurrences


newIndex :: Inverted -> Index
newIndex i =
    Ix
    {
    -- | Returns the number of unique words in the index.
      _sizeWords              = sizeWords' i

    -- | Returns a list of all contexts avaliable in the index.
    , _contexts               = contexts' i

    -- | Returns the occurrences for every word. A potentially expensive operation.
    , _allWords               = allWords' i

    -- | Searches for words beginning with the prefix in a given context (case-sensitive).
    , _prefixCase             = prefixCase' i

    -- | Searches for words beginning with the prefix in a given context (case-insensitive).
    , _prefixNoCase           = prefixNoCase' i

    -- | Searches for and exact word in a given context (case-sensitive).
    , _lookupCase             = lookupCase' i

    -- | Searches for and exact word in a given context (case-insensitive).
    , _lookupNoCase           = lookupNoCase' i

    -- | Insert occurrences.
    , _insertOccurrences      = \c w o -> newIndex $ insertOccurrences' c w o i

    -- | Delete occurrences.
    , _deleteOccurrences      = \c w o -> newIndex $ deleteOccurrences' c w o i

    -- | Insert a position for a single document.
    , _insertPosition         = \c w d p -> newIndex $ insertPosition' c w d p i
    --insertPosition c w d p i      = insertOccurrences c w (singletonOccurrence d p) Index

    -- | Delete a position for a single document.
    , _deletePosition         = \c w d p -> newIndex $ deletePosition' c w d p i
    --deletePosition c w d p i      = deleteOccurrences c w (singletonOccurrence d p) Index

    -- | Delete documents completely (all occurrences).
    , _deleteDocsById         = \ds -> newIndex $ deleteDocsById' ds i

    -- FIXME: no way to access the internal structure without exposing it with the Index data type!?
    -- | Merges two indexes.
    --, _mergeIndexes           = undefined

    -- | Substract one index from another.
    --, _substractIndexes       = undefined -- s.o.

    -- | Splitting an index by its contexts.
    , _splitByContexts        = map newIndex . splitByContexts' i

    -- | Splitting an index by its documents.
    , _splitByDocuments       = map newIndex . splitByDocuments' i

    -- | Splitting an index by its words.
    , _splitByWords           = map newIndex . splitByWords' i

    -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    , _updateDocIds           = undefined

    -- | Update document id's with a simple injective editing function.
    , _updateDocIds'          = undefined
    --updateDocIds' f               = updateDocIds (const . const $ f)

    -- Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations

    , _toList                 = undefined
}


emptyIndex                        :: Index
emptyIndex                        = newIndex (Inverted M.empty)

-- ----------------------------------------------------------------------------

liftInv                           :: (Parts -> Parts) -> Inverted -> Inverted
liftInv f                         = Inverted . f . indexParts

-- | Create an empty index.
emptyInverted                     :: Inverted
emptyInverted                     = Inverted M.empty

-- | Create an index with just one word in one context.
singleton                         :: Context -> Text -> Occurrences -> Inverted
singleton c w o                   = Inverted (M.singleton c (PT.singleton (T.unpack w) (deflateOcc o)))

-- | Merge two sets of index parts.
mergeParts                        :: Parts -> Parts -> Parts
mergeParts                        = M.unionWith mergePart

-- | Merge two index parts.
mergePart                         :: Part -> Part -> Part
mergePart                         = PT.unionWith mergeDiffLists
  where
  mergeDiffLists o1 o2            = deflateOcc $
                                  mergeOccurrences (inflateOcc o1) (inflateOcc o2)

-- | Substract a set of index parts from another.
substractParts                    :: Parts -> Parts -> Parts
substractParts                    = M.differenceWith substractPart

-- | Substract one index part from another.
substractPart                     :: Part -> Part -> Maybe Part
substractPart p1 p2               = if PT.null diffPart then Nothing else Just diffPart
  where
  diffPart                        = PT.differenceWith substractDiffLists p1 p2
    where
    substractDiffLists o1 o2      = if diffOcc == emptyOccurrences then Nothing else Just (deflateOcc diffOcc)
      where
      diffOcc                     = substractOccurrences (inflateOcc o1) (inflateOcc o2)

-- | Internal split function used by the split functions from the HolIndex interface (above).
splitInternal                     :: [(Int, Inverted)] -> Int -> [Inverted]
splitInternal inp n               = allocate mergeIndexes' stack buckets
  where
  buckets                         = zipWith const (createBuckets n) stack
  stack                           = reverse (sortBy (compare `on` fst) inp)

-- | Allocates values from the first list to the buckets in the second list.
allocate                          :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
allocate _ _ []                   = []
allocate _ [] ys                  = map snd ys
allocate f (x:xs) (y:ys)          = allocate f xs (sortBy (compare `on` fst) (combine x y : ys))
  where
  combine (s1, v1) (s2, v2)       = (s1 + s2, f v1 v2)

-- | Create empty buckets for allocating indexes.
createBuckets                     :: Int -> [(Int, Inverted)]
createBuckets n                   = replicate n (0, emptyInverted)

-- | Return a part of the index for a given context.
getPart                           :: Context -> Inverted -> Part
getPart c i                       = fromMaybe PT.empty (M.lookup c $ indexParts i)

-- ----------------------------------------------------------------------------

sizeWords'                        :: Inverted -> Int
sizeWords'                        = M.fold ((+) . PT.size) 0 . indexParts

contexts'                         :: Inverted -> [Context]
contexts'                         = map fst . M.toList . indexParts

allWords'                         :: Inverted -> Context -> RawResult
allWords' i c                     = map (T.pack *** inflateOcc)  $ PT.toList                                  $ getPart c i

prefixCase'                       :: Inverted -> Context -> Text -> [(Text, Occurrences)]
prefixCase' i c q                 = map (T.pack *** inflateOcc)  $ PT.prefixFindWithKey (T.unpack q)          $ getPart c i

prefixNoCase'                     :: Inverted -> Context -> Text -> [(Text, Occurrences)]
prefixNoCase' i c q               = map (T.pack *** inflateOcc)  $ PT.prefixFindNoCaseWithKey (T.unpack q)    $ getPart c i

lookupCase'                       :: Inverted -> Context -> Text -> [(Text, Occurrences)]
lookupCase' i c q                 = map ((,) q .  inflateOcc)    $ maybeToList . PT.lookup (T.unpack q)       $ getPart c i

lookupNoCase'                     :: Inverted -> Context -> Text -> [(Text, Occurrences)]
lookupNoCase' i c q               = map (T.pack *** inflateOcc)  $ PT.lookupNoCase (T.unpack q)               $ getPart c i

mergeIndexes'                     :: Inverted -> Inverted -> Inverted
mergeIndexes' i1 i2               = Inverted (mergeParts (indexParts i1) (indexParts i2))

substractIndexes'                 :: Inverted -> Inverted -> Inverted
substractIndexes' i1 i2           = Inverted (substractParts (indexParts i1) (indexParts i2))

insertOccurrences'                :: Context -> Text -> Occurrences -> Inverted -> Inverted
insertOccurrences' c w o          = mergeIndexes' (singleton c w o)

deleteOccurrences'                :: Context -> Text -> Occurrences -> Inverted -> Inverted
deleteOccurrences' c w o i        = substractIndexes' i (singleton c w o)

splitByContexts'                  :: Inverted -> Int -> [Inverted]
splitByContexts' (Inverted ps)    = splitInternal (map (uncurry annotate) . M.toList $ ps)
  where
  annotate c p  = let i = Inverted (M.singleton c p)
                  in (sizeWords' i, i)

splitByDocuments'                 :: Inverted -> Int -> [Inverted]
splitByDocuments' i               = let
                                    unionDocs' = M.unionWith (M.unionWith unionPos)
                                    docResults = map (\c -> resultByDocument c (allWords' i c)) (contexts' i)
                                    convert (d, cs)             = foldl' makeIndex (0, emptyInverted) (M.toList cs)
                                        where
                                        makeIndex r (c, ws)       = foldl' makeOcc r (M.toList ws)
                                          where
                                          makeOcc (rs, ri) (w, p) = (sizePos p + rs , insertOccurrences' c w (DM.singleton d p) ri)
                                    in splitInternal ( map convert $
                                                    DM.toList $
                                                    DM.unionsWith unionDocs' docResults
                                                  )

splitByWords'                     :: Inverted -> Int -> [Inverted]
splitByWords' i                   = splitInternal indexes
  where
  indexes                         = map convert $
                                    M.toList $
                                    M.unionsWith (M.unionWith mergeOccurrences) wordResults
    where
    wordResults                   = map (\c -> resultByWord c (allWords' i c)) (contexts' i)
    convert (w, cs)               = foldl' makeIndex (0, emptyInverted) (M.toList cs)
      where
      makeIndex (rs, ri) (c, o)   = (rs + sizeOccurrences o, insertOccurrences' c w o ri)

updateDocIdsX                     :: (Context -> Text -> DocId -> DocId) -> Inverted -> Inverted
updateDocIdsX f (Inverted parts)
                                  = Inverted (M.mapWithKey updatePart parts)
  where
  updatePart c                    = PT.mapWithKey
                                    (\w o -> DM.foldWithKey (updateDocument c w) DM.empty o)
  updateDocument c w d            = DM.insertWith mergePositions (f c (T.pack w) d)
    where
    mergePositions p1 p2          = deflatePos $ unionPos (inflatePos p1) (inflatePos p2)

updateDocIdsX'                    :: (DocId -> DocId) -> Inverted -> Inverted
updateDocIdsX' f
                                  = Inverted . M.map updatePart . indexParts
  where
  updatePart                      = PT.map updateOcc
  updateOcc                       = DM.foldWithKey updateId DM.empty
  updateId                        = DM.insert . f

toList' :: Inverted -> [(Context, Text, Occurrences)]
toList' i                         = concatMap convertPart . M.toList $ indexParts i
  where convertPart (c,p)         = map (\(w, o) -> (c, T.pack w, inflateOcc o)) .
                                    PT.toList $ p

fromList'                         :: [(Context, Text, Occurrences)] -> Inverted
fromList'                         = foldl (\i (c,w,o) -> insertOccurrences' c w o i) emptyInverted

deleteDocsById'                   :: Set DocId -> Inverted -> Inverted
deleteDocsById' docIds            = liftInv $ M.mapMaybe deleteInParts
  where
  deleteInParts :: Part -> Maybe Part
  deleteInParts p
    = let p' = PT.mapMaybe deleteInPT p
      in if PT.null p'
            then Nothing
            else return p'
  deleteInPT :: CompressedOccurrences -> Maybe CompressedOccurrences
  deleteInPT occ
    = let occ' = C.differenceWithKeySet docIds occ
      in if DM.null occ'
            then Nothing
            else return occ'

-- XXX: default implementations
insertPosition'                   :: Context -> Word -> DocId -> Position -> Inverted -> Inverted
insertPosition' c w d p           = insertOccurrences' c w (singletonOccurrence d p)

deletePosition'                   :: Context -> Word -> DocId -> Position -> Inverted -> Inverted
deletePosition' c w d p           = deleteOccurrences' c w (singletonOccurrence d p)