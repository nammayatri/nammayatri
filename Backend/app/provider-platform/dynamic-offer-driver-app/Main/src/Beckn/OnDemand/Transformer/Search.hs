{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified Beckn.Types.Core.Taxi.Common.Address
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Types as Spec
-- import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.Utils as Utils
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search
import qualified Domain.Action.Internal.Estimate as DBppEstimate
import qualified Domain.Types.RiderPreferredOption as DRPO
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import qualified Kernel.Types.App
import qualified Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber
import Kernel.Utils.Common (decodeFromText, fromMaybeM, type (:::))
import Kernel.Utils.Logging (logDebug)
import qualified Kernel.Utils.Version
import Tools.Error

buildSearchReq :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text], EncFlow m r) => Data.Text.Text -> Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.SearchReqMessage -> BecknV2.OnDemand.Types.Context -> m Domain.Action.Beckn.Search.DSearchReq
buildSearchReq messageId subscriber req context = do
  now <- Kernel.Types.Common.getCurrentTime
  let bapId_ = subscriber.subscriber_id
      bapUri_ = subscriber.subscriber_url
      customerLanguage_ = Beckn.OnDemand.Utils.Search.buildCustomerLanguage req
      customerNammaTags_ = Beckn.OnDemand.Utils.Search.buildCustomerNammaTags req
      isDashboardRequest_ = Beckn.OnDemand.Utils.Search.checkIfDashboardSearch req
      device_ = Nothing
      disabilityTag_ = Beckn.OnDemand.Utils.Search.buildDisabilityTag req
      isReallocationEnabled_ = Beckn.OnDemand.Utils.Search.getIsReallocationEnabled req
      isMeterRideSearch = Beckn.OnDemand.Utils.Search.getIsMeterRideSearch req
      numberOfLuggages = Beckn.OnDemand.Utils.Search.getNumberOfLuggages req
      fareParametersInRateCard_ = Beckn.OnDemand.Utils.Search.fareParametersInRateCard req
      messageId_ = messageId
      routeDistance_ = Beckn.OnDemand.Utils.Search.getDistance req
      routeDuration_ = Beckn.OnDemand.Utils.Search.getDuration req
      returnTime_ = Beckn.OnDemand.Utils.Search.getReturnTime req
      isRoundTrip_ = Beckn.OnDemand.Utils.Search.getRoundTrip req
      routePoints_ = Beckn.OnDemand.Utils.Search.buildRoutePoints req
      multipleRoutes = Beckn.OnDemand.Utils.Search.buildMultipleRoutesTag req
      driverIdentifier = Beckn.OnDemand.Utils.Search.getDriverIdentifier req
      pickupTime_ = fromMaybe now $ Beckn.OnDemand.Utils.Search.getPickUpTime req
      isMultimodalSearch = Beckn.OnDemand.Utils.Search.getIsMultimodalSearch req
      isReserveRide = getIsReserveRide req
      reserveRideEstimate = getReserveRideEstimate req isReserveRide
      paymentMode = Beckn.OnDemand.Utils.Search.getPaymentMode req
      fromSpecialLocationId_ = getFromSpecialLocationId req
      toSpecialLocationId_ = getToSpecialLocationId req
      userClientDevice = Beckn.OnDemand.Utils.Search.buildUserClientDevice req
      userBackendAppVersion = Beckn.OnDemand.Utils.Search.buildUserBackendAppVersion req
  userBundleVersion <- mapM Kernel.Utils.Version.readVersion (Beckn.OnDemand.Utils.Search.buildUserBundleVersion req)
  userSdkVersion <- mapM Kernel.Utils.Version.readVersion (Beckn.OnDemand.Utils.Search.buildUserSdkVersion req)
  riderPreferredOption <- case Beckn.OnDemand.Utils.Search.buildRiderPreferredOption req of
    Nothing -> pure DRPO.OneWay
    Just txt -> case readMaybe (T.unpack txt) of
      Just val -> pure val
      Nothing -> do
        logDebug $ "Invalid riderPreferredOption value: " <> txt <> ", defaulting to OneWay"
        pure DRPO.OneWay
  bapCountry_ <- Beckn.OnDemand.Utils.Common.getContextCountry context
  customerPhoneNum_ <- getPhoneNumberFromTag $ Beckn.OnDemand.Utils.Search.buildCustomerPhoneNumber req
  dropAddrress_ <- Beckn.OnDemand.Utils.Search.getDropOffLocation req & tfAddress
  dropLocation_ <- tfLatLong `mapM` Beckn.OnDemand.Utils.Search.getDropOffLocationGps req
  stopLocations <- Beckn.OnDemand.Utils.Search.getIntermediateStopLocations req
  stops <- buildLocation `mapM` stopLocations
  pickupAddress_ <- Beckn.OnDemand.Utils.Search.getPickUpLocation req >>= (tfAddress . Just)
  pickupLocation_ <- Beckn.OnDemand.Utils.Search.getPickUpLocationGps req >>= tfLatLong
  transactionId_ <- BecknV2.OnDemand.Utils.Common.getTransactionId context
  logDebug $ "Phone Number at bap side is: " <> show (Beckn.OnDemand.Utils.Search.buildCustomerPhoneNumber req)
  logDebug $ "Phone Number at bap side is: " <> show customerPhoneNum_
  pure $
    Domain.Action.Beckn.Search.DSearchReq
      { bapCountry = bapCountry_,
        bapId = bapId_,
        bapUri = bapUri_,
        customerLanguage = customerLanguage_,
        customerNammaTags = customerNammaTags_,
        customerPhoneNum = customerPhoneNum_,
        isDashboardRequest = fromMaybe False isDashboardRequest_,
        roundTrip = fromMaybe False isRoundTrip_,
        returnTime = returnTime_,
        device = device_,
        disabilityTag = disabilityTag_,
        dropAddrress = dropAddrress_,
        dropLocation = dropLocation_,
        isReallocationEnabled = isReallocationEnabled_,
        fareParametersInRateCard = fareParametersInRateCard_,
        messageId = messageId_,
        pickupAddress = pickupAddress_,
        pickupLocation = pickupLocation_,
        pickupTime = pickupTime_,
        routeDistance = routeDistance_,
        routeDuration = routeDuration_,
        routePoints = routePoints_,
        transactionId = transactionId_,
        stops,
        isReserveRide = isReserveRide,
        mbAdditonalChargeCategories = Nothing,
        fromSpecialLocationId = Id <$> fromSpecialLocationId_,
        toSpecialLocationId = Id <$> toSpecialLocationId_,
        ..
      }

-- [door, building, street, area, city, state, areaCode, country]
tfAddress :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Maybe BecknV2.OnDemand.Types.Location -> m (Maybe Beckn.Types.Core.Taxi.Common.Address.Address)
tfAddress Nothing = pure Nothing
tfAddress (Just location) = do
  fullAddress <- location.locationAddress & fromMaybeM (InvalidRequest $ "Location address is missing, location:-" <> show location)
  returnData <- Beckn.OnDemand.Utils.Common.buildAddressFromText fullAddress
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then do
      pure Nothing
    else do
      pure $ Just returnData

tfLatLong :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> m Kernel.External.Maps.LatLong
tfLatLong locationGps = do
  lat_ <- Beckn.OnDemand.Utils.Common.parseLatLong locationGps
  lon_ <- Beckn.OnDemand.Utils.Common.parseLatLong locationGps
  pure $ Kernel.External.Maps.LatLong {lat = Kernel.External.Maps.lat lat_, lon = Kernel.External.Maps.lon lon_}

buildLocation :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Spec.Location -> m Domain.Action.Beckn.Search.DSearchReqLocation
buildLocation location = do
  address <- tfAddress (Just location)
  locationGpsText <- location.locationGps & fromMaybeM (InvalidRequest "Missing Pickup Location")
  latLong <- tfLatLong locationGpsText
  pure $
    Domain.Action.Beckn.Search.DSearchReqLocation
      { address,
        gps = latLong
      }

getIsReserveRide :: Spec.SearchReqMessage -> Maybe Bool
getIsReserveRide req = do
  intent <- req.searchReqMessageIntent
  fulfillment <- intent.intentFulfillment
  tags <- fulfillment.fulfillmentTags
  readMaybe . T.unpack =<< Utils.getTagV2 Tags.SEARCH_REQUEST_INFO Tags.RESERVED_RIDE_TAG (Just tags)

getReserveRideEstimate :: Spec.SearchReqMessage -> Maybe Bool -> Maybe DBppEstimate.BppEstimate
getReserveRideEstimate req isReserveRide = do
  if isReserveRide == Just True
    then do
      intent <- req.searchReqMessageIntent
      fulfillment <- intent.intentFulfillment
      tags <- fulfillment.fulfillmentTags
      decodeFromText =<< Utils.getTagV2 Tags.SEARCH_REQUEST_INFO Tags.RESERVED_PRICING_TAG (Just tags)
    else Nothing

getPhoneNumberFromTag :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text], EncFlow m r) => Maybe Text -> m (Maybe Text)
getPhoneNumberFromTag customerPhoneNum_ = do
  case customerPhoneNum_ of
    Just phoneNumber ->
      mapM decrypt $ textToEncryptedHashed phoneNumber
    Nothing -> do
      return Nothing

getFromSpecialLocationId :: Spec.SearchReqMessage -> Maybe Text
getFromSpecialLocationId req = do
  intent <- req.searchReqMessageIntent
  fulfillment <- intent.intentFulfillment
  tags <- fulfillment.fulfillmentTags
  Utils.getTagV2 Tags.SEARCH_REQUEST_INFO Tags.FROM_SPECIAL_LOCATION_ID (Just tags)

getToSpecialLocationId :: Spec.SearchReqMessage -> Maybe Text
getToSpecialLocationId req = do
  intent <- req.searchReqMessageIntent
  fulfillment <- intent.intentFulfillment
  tags <- fulfillment.fulfillmentTags
  Utils.getTagV2 Tags.SEARCH_REQUEST_INFO Tags.TO_SPECIAL_LOCATION_ID (Just tags)
