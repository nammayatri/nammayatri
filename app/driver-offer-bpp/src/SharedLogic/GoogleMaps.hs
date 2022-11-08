{-# LANGUAGE OverloadedStrings #-}

module SharedLogic.GoogleMaps where

import Beckn.External.GoogleMaps.Types as GoogleMaps hiding (Address)
import Beckn.Prelude hiding (const, error, getField, setField)
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.App (MonadFlow)
import Data.HashMap.Strict as HashMap hiding (map)
import Data.Text as T hiding (dropWhile, foldl, head, init, length, map, zip)

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
  let resultsResp = head placeNameResp.results
  let hashMap = iterateAddrResp resultsResp.address_components
  let hmTypeAddr = iterateResultResp placeNameResp.results
  let areaAddrComp = HashMap.lookup "sublocality_level_2" hmTypeAddr
  let hmArea = maybe initial iterateAddrResp areaAddrComp
  pure
    Address
      { areaCode = getField ["postal_code"] hashMap,
        street = getField ["route", "street_address"] hashMap,
        city = getField ["locality"] hashMap,
        state = getField ["administrative_area_level_1"] hashMap,
        country = getField ["country"] hashMap,
        building = getField ["premise", "sub_premise"] hashMap,
        area = getField ["sublocality_level_2"] hmArea,
        full_address = resultsResp.formatted_address,
        ..
      }

iterateResultResp :: [ResultsResp] -> HashMap Text [AddressResp]
iterateResultResp = foldl iterateResultTypes initial

iterateResultTypes :: HashMap Text [AddressResp] -> ResultsResp -> HashMap Text [AddressResp]
iterateResultTypes prevMap resultObj = foldl (insertAddrComp resultObj.address_components) prevMap resultObj.types

insertAddrComp :: [AddressResp] -> HashMap Text [AddressResp] -> Text -> HashMap Text [AddressResp]
insertAddrComp address_components prevMap typeName = HashMap.insert typeName address_components prevMap

iterateAddrResp :: [AddressResp] -> HashMap Text Text
iterateAddrResp = foldl iterateAddrTypes initial

iterateAddrTypes :: HashMap Text Text -> AddressResp -> HashMap Text Text
iterateAddrTypes prevMap addressObj = foldl (insertTypeName addressObj.long_name) prevMap addressObj.types

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
