{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module SharedLogic.GoogleMaps where

import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as T hiding (dropWhile, foldl, head, init, length, map, zip)
import Kernel.External.Maps.Interface.Types
import Kernel.Prelude hiding (const, error, getField, setField)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App (MonadFlow)

data Address = Address
  { street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    full_address :: Maybe Text
  }
  deriving (Show, Generic)

mkLocation :: (MonadFlow m, CoreMetrics m) => GetPlaceNameResp -> m Address
mkLocation placeNameResp = do
  let resultsResp = head placeNameResp
  let hashMap = iterateAddrResp resultsResp.addressComponents
  pure
    Address
      { areaCode = getField ["postal_code"] hashMap,
        street = getField ["route", "street_address"] hashMap,
        city = getField ["locality"] hashMap,
        state = getField ["administrative_area_level_1"] hashMap,
        country = getField ["country"] hashMap,
        building = getField ["premise", "sub_premise"] hashMap,
        area = getField ["sublocality_level_5", "sublocality_level_4", "sublocality_level_3", "sublocality_level_2", "sublocality_level_1"] hashMap <|> getField ["sublocality"] hashMap,
        full_address = resultsResp.formattedAddress
      }

iterateAddrResp :: [AddressResp] -> HashMap Text Text
iterateAddrResp = foldl iterateAddrTypes initial

iterateAddrTypes :: HashMap Text Text -> AddressResp -> HashMap Text Text
iterateAddrTypes prevMap addressObj = foldl (insertTypeName addressObj.longName) prevMap addressObj.types

insertTypeName :: Text -> HashMap Text Text -> Text -> HashMap Text Text
insertTypeName long_name prevMap typeName = HashMap.insert typeName long_name prevMap

initial :: HashMap k v
initial = HashMap.empty

getField :: [Text] -> HashMap Text Text -> Maybe Text
getField fields hashMap = formatFields $ map (`HashMap.lookup` hashMap) fields

formatFields :: [Maybe Text] -> Maybe Text
formatFields [] = Just (pack "")
formatFields [Nothing] = Just (pack "")
formatFields [Just x] = Just x
formatFields (Nothing : xs) = formatFields xs
formatFields (x : xs) = formatFields [x] <> Just (pack ",") <> formatFields xs
