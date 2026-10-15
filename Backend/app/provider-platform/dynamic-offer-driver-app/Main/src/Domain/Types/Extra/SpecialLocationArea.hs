{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.SpecialLocationArea where

import Data.Aeson.Types (FromJSONKey (..), FromJSONKeyFunction (..), ToJSONKey (..), toJSONKeyText)
import qualified Data.Text as T
import Kernel.Prelude
import qualified Lib.Types.SpecialLocation as SL

-- | Encodes a 'Map SL.Area v' as a JSON object keyed by the area's show form
-- ("Default", "Pickup_<id>", "Drop_<id>", ...) instead of a list of pairs.
instance ToJSONKey SL.Area where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey SL.Area where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
      case readMaybe (T.unpack t) of
        Just area -> pure area
        Nothing -> fail $ "unknown Area: " <> T.unpack t
