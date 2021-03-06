{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Text index using the 'DocIdMap' based on the 'StringMap' implementation.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.PrefixTreeIndex2Dim
( DmPrefixTree(..)
)
where

import           Control.DeepSeq

import           Data.Binary               (Binary (..))
import           Data.Typeable

import qualified Data.StringMap.Dim2Search as SM2
import qualified Data.StringMap.Strict     as SM

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdMap      as DM
import           Hunt.Index

import           Hunt.Utility

-- ------------------------------------------------------------

-- | Text index using 'DocIdMap' based on the 'StringMap' implementation.
--   Note that the value parameter is on the type of the 'DocIdMap' value and not the 'Occurrences'
--   itself.
newtype DmPrefixTree v
  = DmPT { dmPT :: SM.StringMap (DocIdMap v) }
  deriving (Eq, Show, NFData, Typeable)

mkDmPT :: NFData v => SM.StringMap (DocIdMap v) -> DmPrefixTree v
mkDmPT v = DmPT $! v

-- ------------------------------------------------------------

instance (NFData v,Binary v) => Binary (DmPrefixTree v) where
  put = put . dmPT
  get = get >>= return . mkDmPT

-- ------------------------------------------------------------

instance Index DmPrefixTree where
  type IKey DmPrefixTree v = SM.Key
  type IVal DmPrefixTree v = DocIdMap v

  insertList op kvs (DmPT pt) =
    mkDmPT $ SM.unionWith op pt (SM.fromList kvs)

  deleteDocs ks (DmPT pt)
    = mkDmPT $ SM.mapMaybe (\m -> let dm = DM.diffWithSet m ks
                                  in if DM.null dm then Nothing else Just dm) pt

  empty
    = mkDmPT $ SM.empty

  fromList
    = mkDmPT . SM.fromList

  toList (DmPT pt)
    = SM.toList pt

  search t k (DmPT pt)
    = case t of
        Case         -> case SM.lookup k pt of
                          Nothing -> []
                          Just xs -> [(k,xs)]
        NoCase       -> luCase k pt
        PrefixCase   -> pfCase k pt
        PrefixNoCase -> pfNoCase k pt
    where
    toL      = SM.toListShortestFirst
    luCase   = toL .:: SM.lookupNoCase
    pfCase   = toL .:: SM.prefixFilter
    pfNoCase = toL .:: SM.prefixFilterNoCase

  lookupRange k1 k2 (DmPT pt)
    = SM.toList $ SM2.lookupRange k1 k2 pt

  unionWith op (DmPT pt1) (DmPT pt2)
    = mkDmPT $ SM.unionWith op pt1 pt2

{-
  unionWithConv to f (DmPT i1) (DmPT i2)
    = liftM mkDmPT $ unionWithConv to f i1 i2
-}

  map f (DmPT pt)
    = mkDmPT $ SM.map f pt

  mapMaybe f (DmPT pt)
    = mkDmPT $ SM.mapMaybe f pt

  keys (DmPT pt)
    = SM.keys pt

-- ------------------------------------------------------------
