{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import Beckn.ACL.Common (getTag)
import qualified Beckn.OnDemand.Transformer.Search as TSearch
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified BecknV2.OnDemand.Utils.Context as ContextUtils
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as Merchant
import Kernel.Beam.Functions
import Kernel.External.Maps.Types hiding (geometry)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.Queries.Geometry
import Tools.Error
import qualified Tools.Maps as Maps

buildSearchReqV1 ::
  (HasFlowEnv m r '["coreVersion" ::: Text], CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant.Merchant ->
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReqV1 merchantId subscriber req = do
  now <- getCurrentTime
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
      dropOff = intent.fulfillment.end
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let geoRestriction = merchant.geofencingConfig.origin
  city <-
    case geoRestriction of
      Unrestricted -> pure merchant.city
      Regions regions -> do
        geometry <-
          runInReplica $
            findGeometriesContaining LatLong {lat = pickup.location.gps.lat, lon = pickup.location.gps.lon} regions >>= \case
              [] -> do
                logError $ "No geometry found for pickup: " <> show pickup <> " for regions: " <> show regions
                pure Nothing
              (g : _) -> pure $ Just g
        pure $ fromMaybe merchant.city ((.city) <$> geometry)
  let distance = getDistance =<< intent.fulfillment.tags
  let duration = getDuration =<< intent.fulfillment.tags
  let customerLanguage = buildCustomerLanguage =<< intent.fulfillment.customer
  let isReallocationEnabled = getIsReallocationEnabled =<< intent.fulfillment.tags
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let disabilityTag = buildDisabilityTag =<< intent.fulfillment.customer
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure
    DSearch.DSearchReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        bapCity = city,
        bapCountry = context.country,
        pickupLocation = LatLong {lat = pickup.location.gps.lat, lon = pickup.location.gps.lon},
        pickupTime = now,
        dropLocation = LatLong {lat = dropOff.location.gps.lat, lon = dropOff.location.gps.lon},
        pickupAddress = pickup.location.address,
        dropAddrress = dropOff.location.address,
        routeDistance = distance,
        routeDuration = duration,
        device = Nothing,
        routePoints = buildRoutePoints =<< intent.fulfillment.tags, --------TODO------Take proper input---------
        customerLanguage = customerLanguage,
        disabilityTag = disabilityTag,
        customerPhoneNum = buildCustomerPhoneNumber =<< intent.fulfillment.customer,
        ..
      }

buildSearchReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text], CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant.Merchant ->
  Subscriber.Subscriber ->
  Search.SearchReqV2 ->
  m DSearch.DSearchReq
buildSearchReqV2 merchantId subscriber req = do
  let context = req.searchReqContext
  ContextUtils.validateContext Context.SEARCH context
  messageId <- Utils.getMessageId context
  let message = req.searchReqMessage
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- let geoRestriction = merchant.geofencingConfig.origin
  city <- pure merchant.city -- shrey00 : handle geoRestriction later
  -- case geoRestriction of
  --   Unrestricted -> pure merchant.city
  --   Regions regions -> do
  --     geometry <-
  --       runInReplica $
  --         findGeometriesContaining LatLong {lat = Beckn.OnDemand.Utils.Common.parseLatLong pickup.location.gps & (.lat), lon = Beckn.OnDemand.Utils.Common.parseLatLong pickup.location.gps & (.lon)} regions >>= \case
  --           [] -> do
  --             logError $ "No geometry found for pickup: " <> show pickup <> " for regions: " <> show regions
  --             pure Nothing
  --           (g : _) -> pure $ Just g
  --     pure $ fromMaybe merchant.city ((.city) <$> geometry)
  TSearch.buildSearchReq messageId city subscriber message context

getDistance :: Search.TagGroups -> Maybe Meters
getDistance tagGroups = do
  tagValue <- getTag "route_info" "distance_info_in_m" tagGroups
  distanceValue <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceValue

getDuration :: Search.TagGroups -> Maybe Seconds
getDuration tagGroups = do
  tagValue <- getTag "route_info" "duration_info_in_s" tagGroups
  durationValue <- readMaybe $ T.unpack tagValue
  Just $ Seconds durationValue

getIsReallocationEnabled :: Search.TagGroups -> Maybe Bool
getIsReallocationEnabled tagGroups = do
  tagValue <- getTag "reallocation_info" "is_reallocation_enabled" tagGroups
  readMaybe $ T.unpack tagValue

buildCustomerLanguage :: Search.Customer -> Maybe Language
buildCustomerLanguage Search.Customer {..} = do
  tagValue <- getTag "customer_info" "customer_language" person.tags
  readMaybe $ T.unpack tagValue

buildDisabilityTag :: Search.Customer -> Maybe Text
buildDisabilityTag Search.Customer {..} = do
  tagValue <- getTag "customer_info" "customer_disability" person.tags
  readMaybe $ T.unpack tagValue

buildRoutePoints :: Search.TagGroups -> Maybe [Maps.LatLong]
buildRoutePoints tagGroups = do
  tagValue <- getTag "route_info" "route_points" tagGroups
  decode $ encodeUtf8 tagValue

buildCustomerPhoneNumber :: Search.Customer -> Maybe Text
buildCustomerPhoneNumber Search.Customer {..} = do
  tagValue <- getTag "customer_info" "customer_phone_number" person.tags
  readMaybe $ T.unpack tagValue
