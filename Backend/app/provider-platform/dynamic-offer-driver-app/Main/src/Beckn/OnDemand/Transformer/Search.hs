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
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Text
import qualified Domain.Action.Beckn.Search
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import qualified Kernel.Types.App
import qualified Kernel.Types.Common
import qualified Kernel.Types.Registry.Subscriber
import Kernel.Utils.Common (fromMaybeM, type (:::))
import Tools.Error

buildSearchReq :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.SearchReqMessage -> BecknV2.OnDemand.Types.Context -> m Domain.Action.Beckn.Search.DSearchReq
buildSearchReq messageId subscriber req context = do
  now <- Kernel.Types.Common.getCurrentTime
  let bapId_ = subscriber.subscriber_id
      bapUri_ = subscriber.subscriber_url
      customerLanguage_ = Beckn.OnDemand.Utils.Search.buildCustomerLanguage req
      customerNammaTags_ = Beckn.OnDemand.Utils.Search.buildCustomerNammaTags req
      isDashboardRequest_ = Beckn.OnDemand.Utils.Search.checkIfDashboardSearch req
      customerPhoneNum_ = Beckn.OnDemand.Utils.Search.buildCustomerPhoneNumber req
      device_ = Nothing
      disabilityTag_ = Beckn.OnDemand.Utils.Search.buildDisabilityTag req
      isReallocationEnabled_ = Beckn.OnDemand.Utils.Search.getIsReallocationEnabled req
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
  bapCountry_ <- Beckn.OnDemand.Utils.Common.getContextCountry context
  dropAddrress_ <- Beckn.OnDemand.Utils.Search.getDropOffLocation req & tfAddress
  dropLocation_ <- tfLatLong `mapM` Beckn.OnDemand.Utils.Search.getDropOffLocationGps req
  stopLocations <- Beckn.OnDemand.Utils.Search.getIntermediateStopLocations req
  stops <- buildLocation `mapM` stopLocations
  pickupAddress_ <- Beckn.OnDemand.Utils.Search.getPickUpLocation req >>= (tfAddress . Just)
  pickupLocation_ <- Beckn.OnDemand.Utils.Search.getPickUpLocationGps req >>= tfLatLong
  transactionId_ <- BecknV2.OnDemand.Utils.Common.getTransactionId context
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
    then pure Nothing
    else pure $ Just returnData

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
