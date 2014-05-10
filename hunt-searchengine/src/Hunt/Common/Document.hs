{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  The document representation.

  This includes the

  * URI for identification,
  * the description for the data itself and
  * the weight used in ranking.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Document
where

import           Control.Applicative
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary             (Binary (..))
import           Data.Text               as T
import           Data.Text.Binary        ()

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | The document representation.
data Document = Document
  { uri  :: ! URI         -- ^ Unique identifier of the document.
  , desc :: ! Description -- ^ Description of the document (simple key-value store).
  , wght :: ! Float       -- ^ Weight used in ranking (default @1.0@).
  , score :: Maybe Score
  }
  deriving (Show, Eq, Ord)

-- ------------------------------------------------------------
-- JSON instances implemented with ApiDocument
-- ------------------------------------------------------------

toApiDocument :: Document -> ApiDocument
toApiDocument (Document u d w _)
    = ApiDocument u emptyApiDocIndexMap d (if w == 1.0 then Nothing else Just w)

fromApiDocument :: ApiDocument -> Document
fromApiDocument (ApiDocument u _ix d w)
    = Document u d (maybe 1.0 id w) Nothing

setScore :: Score -> Document -> Document
setScore s d = d {score = Just s}

--instance ToJSON Document where
--    toJSON = toJSON . toApiDocument

instance FromJSON Document where
  parseJSON o = fromApiDocument <$> parseJSON o


instance ToJSON Document where
  toJSON (Document u d w ms) = object $
    [ "uri"         .= u
    , "description" .= d] ++
    (if w /= 1.0 then [] else ["weight"       .= w]) ++ 
    (maybe [] (\s -> (if s /= 1.0 then [] else ["score" .= s])) ms)
    

{-
instance FromJSON Document where
  parseJSON (Object o) = do
    parsedDesc <- o .: "desc"
    parsedUri  <- o .: "uri"
    parsedWght <- o .: "weight"
    return Document
      { uri  = parsedUri
      , desc = parsedDesc
      , wght = parsedWght
      }
  parseJSON _ = mzero
-}

-- ------------------------------------------------------------

instance Binary Document where
  put (Document u d w _) = put u >> put d >> put w
  get = Document <$> get <*> get <*> get <*> (pure Nothing)

instance NFData Document where
  rnf (Document t d w s) = rnf t `seq` rnf d `seq` rnf w `seq` rnf s

-- ------------------------------------------------------------

-- | Simple bijection between @e@ and 'Document' for compression.
class DocumentWrapper e where
  -- | Get the document.
  unwrap :: e -> Document
  -- | Create e from document.
  wrap   :: Document -> e
  -- | Update the wrapped document.
  -- @update f = wrap . f . unwrap@.
  update :: (Document -> Document) -> e -> e
  update f = wrap . f . unwrap

instance DocumentWrapper Document where
  unwrap = id
  wrap   = id

-- ------------------------------------------------------------

instance LogShow Document where
  logShow o = "Document {uri = \"" ++ (T.unpack . uri $ o) ++ "\", ..}"

-- ------------------------------------------------------------
