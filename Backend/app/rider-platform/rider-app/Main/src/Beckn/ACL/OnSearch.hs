{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextUtils
import BecknV2.Utils
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import EulerHS.Prelude hiding (find, id, map, readMaybe, state, unpack)
import Kernel.Prelude (fromJust)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.TimeRFC339 (convertRFC3339ToUTC)
import Kernel.Utils.Common
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Tools.Error

-- import qualified Data.Aeson as A
-- import qualified Data.ByteString as BS
-- import qualified Data.Text as T
-- import Environment

-- parseOnSearch :: IO ()
-- parseOnSearch = do
--   -- let filePath = "/Users/jaypal.m/Desktop/nammayatri-alt/nammayatri/Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/on-search.txt"
--   -- fileContent <- readFile filePath
--   let fileContent = "OnSearchReq {onSearchReqContext = Context {contextAction = Just \"on_search\", contextBapId = Just \"beta.beckn.uat.juspay.net/dev/bap/beckn/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51\", contextBapUri = Just \"https://f57b-13-232-74-226.ngrok-free.app/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52\", contextBppId = Just \"openbox.staging.triffy.in\", contextBppUri = Just \"https://openbox.staging.triffy.in/seller\", contextDomain = Just \"ONDC:TRV10\", contextKey = Nothing, contextLocation = Just (Location {locationAddress = Nothing, locationAreaCode = Nothing, locationCity = Just (City {cityCode = Just \"std:040\", cityName = Nothing}), locationCountry = Just (Country {countryCode = Just \"IND\", countryName = Nothing}), locationGps = Nothing, locationId = Nothing, locationState = Nothing, locationUpdatedAt = Nothing}), contextMessageId = Just 95ff4f64-37e7-4fab-9424-8f67e2f3a495, contextTimestamp = Just (UTCTimeRFC3339 2024-03-06 08:53:44.041 UTC), contextTransactionId = Just bc5b7f74-8616-422f-9de9-84a89ba5093f, contextTtl = Just \"PT300S\", contextVersion = Just \"2.0.0\"}, onSearchReqError = Nothing, onSearchReqMessage = Just (OnSearchReqMessage {onSearchReqMessageCatalog = Catalog {catalogDescriptor = Just (Descriptor {descriptorCode = Nothing, descriptorName = Just \"Yaary Ride Hailing\", descriptorShortDesc = Nothing}), catalogProviders = Just [Provider {providerDescriptor = Just (Descriptor {descriptorCode = Nothing, descriptorName = Just \"Yaary Ride Hailing\", descriptorShortDesc = Nothing}), providerFulfillments = Just [Fulfillment {fulfillmentAgent = Nothing, fulfillmentCustomer = Nothing, fulfillmentId = Just \"2d88e966-7bdc-400c-bce3-8b3ee1d7fe6d\", fulfillmentState = Nothing, fulfillmentStops = Just [Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Buildings, 18th Main, 8th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.401798, 78.427944\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Just (Time {timeDuration = Nothing, timeTimestamp = Just 2024-03-06 08:53:42.308378 UTC}), stopType = Just \"START\"},Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Apartments, 18th Main, 6th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.445692, 78.437557\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Nothing, stopType = Just \"END\"}], fulfillmentTags = Just [TagGroup {tagGroupDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_INFO\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagGroupDisplay = Just True, tagGroupList = Just [Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_DISTANCE\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"9.30\"},Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_TIME\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"31.3\"}]}], fulfillmentType = Just \"DELIVERY\", fulfillmentVehicle = Just (Vehicle {vehicleCategory = Just \"CAB\", vehicleColor = Nothing, vehicleMake = Nothing, vehicleModel = Nothing, vehicleRegistration = Nothing, vehicleVariant = Just \"HATCHBACK\"})},Fulfillment {fulfillmentAgent = Nothing, fulfillmentCustomer = Nothing, fulfillmentId = Just \"6d17a78d-e5fe-4c9c-976c-7bc02b51349b\", fulfillmentState = Nothing, fulfillmentStops = Just [Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Buildings, 18th Main, 8th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.401798, 78.427944\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Just (Time {timeDuration = Nothing, timeTimestamp = Just 2024-03-06 08:53:42.308378 UTC}), stopType = Just \"START\"},Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Apartments, 18th Main, 6th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.445692, 78.437557\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Nothing, stopType = Just \"END\"}], fulfillmentTags = Just [TagGroup {tagGroupDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_INFO\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagGroupDisplay = Just True, tagGroupList = Just [Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_DISTANCE\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"9.30\"},Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_TIME\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"31.3\"}]}], fulfillmentType = Just \"DELIVERY\", fulfillmentVehicle = Just (Vehicle {vehicleCategory = Just \"CAB\", vehicleColor = Nothing, vehicleMake = Nothing, vehicleModel = Nothing, vehicleRegistration = Nothing, vehicleVariant = Just \"SEDAN\"})},Fulfillment {fulfillmentAgent = Nothing, fulfillmentCustomer = Nothing, fulfillmentId = Just \"ad80a717-566c-4a40-b2c0-756af1bf73d2\", fulfillmentState = Nothing, fulfillmentStops = Just [Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Buildings, 18th Main, 8th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.401798, 78.427944\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Just (Time {timeDuration = Nothing, timeTimestamp = Just 2024-03-06 08:53:42.308378 UTC}), stopType = Just \"START\"},Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Apartments, 18th Main, 6th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.445692, 78.437557\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Nothing, stopType = Just \"END\"}], fulfillmentTags = Just [TagGroup {tagGroupDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_INFO\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagGroupDisplay = Just True, tagGroupList = Just [Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_DISTANCE\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"9.30\"},Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_TIME\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"31.3\"}]}], fulfillmentType = Just \"DELIVERY\", fulfillmentVehicle = Just (Vehicle {vehicleCategory = Just \"CAB\", vehicleColor = Nothing, vehicleMake = Nothing, vehicleModel = Nothing, vehicleRegistration = Nothing, vehicleVariant = Just \"SUV\"})},Fulfillment {fulfillmentAgent = Nothing, fulfillmentCustomer = Nothing, fulfillmentId = Just \"3e174cb6-8d6d-4c6d-9c81-ac6637fbe4b2\", fulfillmentState = Nothing, fulfillmentStops = Just [Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Buildings, 18th Main, 8th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.401798, 78.427944\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Just (Time {timeDuration = Nothing, timeTimestamp = Just 2024-03-06 08:53:42.308378 UTC}), stopType = Just \"START\"},Stop {stopAuthorization = Nothing, stopLocation = Just (Location {locationAddress = Just \"#444, Juspay Apartments, 18th Main, 6th Block Koramangala, Bangalore, Karnataka, India\", locationAreaCode = Just \"560047\", locationCity = Just (City {cityCode = Nothing, cityName = Just \"Bangalore\"}), locationCountry = Just (Country {countryCode = Nothing, countryName = Just \"India\"}), locationGps = Just \"17.445692, 78.437557\", locationId = Nothing, locationState = Just (State {stateName = Just \"Karnataka\"}), locationUpdatedAt = Nothing}), stopTime = Nothing, stopType = Just \"END\"}], fulfillmentTags = Just [TagGroup {tagGroupDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_INFO\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagGroupDisplay = Just True, tagGroupList = Just [Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_DISTANCE\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"9.30\"},Tag {tagDescriptor = Just (Descriptor {descriptorCode = Just \"ROUTE_TIME\", descriptorName = Nothing, descriptorShortDesc = Nothing}), tagDisplay = Nothing, tagValue = Just \"31.3\"}]}], fulfillmentType = Just \"DELIVERY\", fulfillmentVehicle = Just (Vehicle {vehicleCategory = Just \"AUTO_RICKSHAW\", vehicleColor = Nothing, vehicleMake = Nothing, vehicleModel = Nothing, vehicleRegistration = Nothing, vehicleVariant = Just \"AUTO\"})}], providerId = Just \"yaary_provider\", providerItems = Just [Item {itemDescriptor = Just (Descriptor {descriptorCode = Just \"RIDE\", descriptorName = Just \"TRIFFY RIDE\", descriptorShortDesc = Nothing}), itemFulfillmentIds = Just [\"2d88e966-7bdc-400c-bce3-8b3ee1d7fe6d\"], itemId = Just \"90c026cc-2664-457e-8700-3dbf9e3e918c\", itemLocationIds = Nothing, itemPaymentIds = Nothing, itemPrice = Just (Price {priceComputedValue = Nothing, priceCurrency = Just \"INR\", priceMaximumValue = Nothing, priceMinimumValue = Nothing, priceOfferedValue = Nothing, priceValue = Just \"163\"}), itemTags = Nothing},Item {itemDescriptor = Just (Descriptor {descriptorCode = Just \"RIDE\", descriptorName = Just \"TRIFFY RIDE\", descriptorShortDesc = Nothing}), itemFulfillmentIds = Just [\"6d17a78d-e5fe-4c9c-976c-7bc02b51349b\"], itemId = Just \"d1a2ee5b-988b-4586-8c58-fa86177e862f\", itemLocationIds = Nothing, itemPaymentIds = Nothing, itemPrice = Just (Price {priceComputedValue = Nothing, priceCurrency = Just \"INR\", priceMaximumValue = Nothing, priceMinimumValue = Nothing, priceOfferedValue = Nothing, priceValue = Just \"173\"}), itemTags = Nothing},Item {itemDescriptor = Just (Descriptor {descriptorCode = Just \"RIDE\", descriptorName = Just \"TRIFFY RIDE\", descriptorShortDesc = Nothing}), itemFulfillmentIds = Just [\"ad80a717-566c-4a40-b2c0-756af1bf73d2\"], itemId = Just \"76b4f9f5-ec69-4069-a983-9d826c1c2984\", itemLocationIds = Nothing, itemPaymentIds = Nothing, itemPrice = Just (Price {priceComputedValue = Nothing, priceCurrency = Just \"INR\", priceMaximumValue = Nothing, priceMinimumValue = Nothing, priceOfferedValue = Nothing, priceValue = Just \"183\"}), itemTags = Nothing},Item {itemDescriptor = Just (Descriptor {descriptorCode = Just \"RIDE\", descriptorName = Just \"TRIFFY RIDE\", descriptorShortDesc = Nothing}), itemFulfillmentIds = Just [\"3e174cb6-8d6d-4c6d-9c81-ac6637fbe4b2\"], itemId = Just \"2cd61922-2177-43c1-b195-6c24a71c56d3\", itemLocationIds = Nothing, itemPaymentIds = Nothing, itemPrice = Just (Price {priceComputedValue = Nothing, priceCurrency = Just \"INR\", priceMaximumValue = Nothing, priceMinimumValue = Nothing, priceOfferedValue = Nothing, priceValue = Just \"194\"}), itemTags = Nothing}], providerLocations = Nothing, providerPayments = Nothing}]}})}"
-- let mbOnSearchReq = readMaybe $ T.unpack fileContent
-- whenJust mbOnSearchReq $ \(onSearchReq :: Spec.OnSearchReq) -> do
--   -- putStrLn $ "onSearchReq:-" <> (show onSearchReq :: Text)
--   _res <- liftIO $ buildOnSearchReqV2 onSearchReq
--   pure ()
-- putStrLn ("res generated" :: Text)

buildOnSearchReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r
  ) =>
  Spec.OnSearchReq ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReqV2 req = do
  ContextUtils.validateContext Context.ON_SEARCH req.onSearchReqContext
  logOnSearchEventV2 req
  onSearchTtl <- req.onSearchReqContext.contextTtl & fromMaybeM (InvalidRequest "Missing ttl")
  timestamp <- req.onSearchReqContext.contextTimestamp >>= Just . convertRFC3339ToUTC & fromMaybeM (InvalidRequest "Missing timestamp")
  let validTill = addDurationToUTCTime timestamp (fromJust (parseISO8601Duration onSearchTtl))
  case req.onSearchReqError of
    Nothing -> do
      message <- req.onSearchReqMessage & fromMaybeM (InvalidRequest "Missing message")
      let catalog = message.onSearchReqMessageCatalog
      providers <- catalog.catalogProviders & fromMaybeM (InvalidRequest "Missing Providers")
      provider <- safeHead providers & fromMaybeM (InvalidRequest "Missing Provider")
      fulfillments <- provider.providerFulfillments & fromMaybeM (InvalidRequest "Missing Fulfillments")
      items <- provider.providerItems & fromMaybeM (InvalidRequest "Missing Items")
      Just <$> TOnSearch.buildOnSearchReq req provider items fulfillments validTill
    Just err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

logOnSearchEventV2 :: EsqDBFlow m r => Spec.OnSearchReq -> m ()
logOnSearchEventV2 req = do
  let context = req.onSearchReqContext
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- Utils.getContextBppId context
  messageId <- Utils.getMessageIdText context
  (errorCode, errorMessage, errorType) <- case req.onSearchReqError of
    Just err -> do
      let errorCode = err.errorCode
      let errorMessage = err.errorMessage
      let errorType = err.errorMessage
      return (errorCode, errorMessage, errorType)
    Nothing ->
      return (Nothing, Nothing, Nothing)
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}
