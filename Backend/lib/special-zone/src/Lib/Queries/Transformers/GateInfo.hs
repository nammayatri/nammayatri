{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.Transformers.GateInfo where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude

-- | The `point` column historically stored a serialized 'LatLong' produced by
--   persistent's @derivePersistField@ (i.e. @T.pack . show@). We preserve that
--   exact representation so existing rows continue to read/write correctly.
latLongToText :: LatLong -> Text
latLongToText = T.pack . show

mkLatLongFromText :: Text -> LatLong
mkLatLongFromText = read . T.unpack

-- | Decode the JSON text column into a per-variant threshold map.
--   Returns Nothing on missing/invalid JSON so we can fall back to defaults.
decodeThresholdMap :: Maybe Text -> Maybe (Map.Map Text Int)
decodeThresholdMap Nothing = Nothing
decodeThresholdMap (Just t) = A.decode (BL.fromStrict (TE.encodeUtf8 t))

-- | Encode a per-variant threshold map back to JSON text for storage.
encodeThresholdMap :: Maybe (Map.Map Text Int) -> Maybe Text
encodeThresholdMap = fmap (TE.decodeUtf8 . BL.toStrict . A.encode)

decodeBoolMap :: Maybe Text -> Maybe (Map.Map Text Bool)
decodeBoolMap Nothing = Nothing
decodeBoolMap (Just t) = A.decode (BL.fromStrict (TE.encodeUtf8 t))

encodeBoolMap :: Maybe (Map.Map Text Bool) -> Maybe Text
encodeBoolMap = fmap (TE.decodeUtf8 . BL.toStrict . A.encode)

decodeTextMap :: Maybe Text -> Maybe (Map.Map Text Text)
decodeTextMap Nothing = Nothing
decodeTextMap (Just t) = A.decode (BL.fromStrict (TE.encodeUtf8 t))

encodeTextMap :: Maybe (Map.Map Text Text) -> Maybe Text
encodeTextMap = fmap (TE.decodeUtf8 . BL.toStrict . A.encode)
