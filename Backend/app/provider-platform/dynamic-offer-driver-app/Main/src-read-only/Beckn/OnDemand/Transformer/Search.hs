{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.Types.Core.Taxi.API.Search
import qualified Beckn.Types.Core.Taxi.Common.Address
import qualified BecknV2.OnDemand.Types
import qualified Data.Text
import qualified Domain.Action.Beckn.Search
import qualified Domain.Types.Merchant
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber
import Kernel.Utils.Common (type (:::))

buildSearchReq :: (Monad m, Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.SearchReqMessage -> BecknV2.OnDemand.Types.Context -> m Domain.Action.Beckn.Search.DSearchReq
buildSearchReq messageId city subscriber req context = do
  let dSearchReqBapCity = city
  let dSearchReqBapId = subscriber.subscriber_id
  let dSearchReqBapUri = subscriber.subscriber_url
  let dSearchReqCustomerLanguage = Beckn.OnDemand.Utils.Common.buildCustomerLanguage req
  let dSearchReqCustomerPhoneNum = Beckn.OnDemand.Utils.Common.buildCustomerPhoneNumber req
  let dSearchReqDevice = Nothing
  let dSearchReqDisabilityTag = Beckn.OnDemand.Utils.Common.buildDisabilityTag req
  let dSearchReqIsReallocationEnabled = Beckn.OnDemand.Utils.Common.getIsReallocationEnabled req
  let dSearchReqMessageId = messageId
  let dSearchReqRouteDistance = Beckn.OnDemand.Utils.Common.getDistance req
  let dSearchReqRouteDuration = Beckn.OnDemand.Utils.Common.getDuration req
  let dSearchReqRoutePoints = Beckn.OnDemand.Utils.Common.buildRoutePoints req
  dSearchReqBapCountry <- Beckn.OnDemand.Utils.Common.getContextCountry context
  dSearchReqDropAddrress <- Beckn.OnDemand.Utils.Common.getDropOffLocation req & tfAddress <&> Just
  dSearchReqDropLocation <- Beckn.OnDemand.Utils.Common.getDropOffLocationGps req & tfLatLong
  dSearchReqPickupAddress <- Beckn.OnDemand.Utils.Common.getPickUpLocation req & tfAddress <&> Just
  dSearchReqPickupLocation <- Beckn.OnDemand.Utils.Common.getPickUpLocationGps req & tfLatLong
  dSearchReqPickupTime <- Kernel.Types.Common.getCurrentTime
  dSearchReqTransactionId <- Beckn.OnDemand.Utils.Common.getTransactionId context
  pure $
    Domain.Action.Beckn.Search.DSearchReq
      { bapCity = dSearchReqBapCity,
        bapCountry = dSearchReqBapCountry,
        bapId = dSearchReqBapId,
        bapUri = dSearchReqBapUri,
        customerLanguage = dSearchReqCustomerLanguage,
        customerPhoneNum = dSearchReqCustomerPhoneNum,
        device = dSearchReqDevice,
        disabilityTag = dSearchReqDisabilityTag,
        dropAddrress = dSearchReqDropAddrress,
        dropLocation = dSearchReqDropLocation,
        isReallocationEnabled = dSearchReqIsReallocationEnabled,
        messageId = dSearchReqMessageId,
        pickupAddress = dSearchReqPickupAddress,
        pickupLocation = dSearchReqPickupLocation,
        pickupTime = dSearchReqPickupTime,
        routeDistance = dSearchReqRouteDistance,
        routeDuration = dSearchReqRouteDuration,
        routePoints = dSearchReqRoutePoints,
        transactionId = dSearchReqTransactionId
      }

tfAddress :: (Monad m, Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => BecknV2.OnDemand.Types.Location -> m Beckn.Types.Core.Taxi.Common.Address.Address
tfAddress location = do
  let addressArea_code = Nothing
  let addressBuilding = Nothing
  let addressCity = Nothing
  let addressCountry = Nothing
  let addressDoor = location.locationAddress
  let addressLocality = Nothing
  let addressState = Nothing
  let addressStreet = Nothing
  let addressWard = Nothing
  pure $
    Beckn.Types.Core.Taxi.Common.Address.Address
      { area_code = addressArea_code,
        building = addressBuilding,
        city = addressCity,
        country = addressCountry,
        door = addressDoor,
        locality = addressLocality,
        state = addressState,
        street = addressStreet,
        ward = addressWard
      }

tfLatLong :: (Monad m, Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> m Kernel.External.Maps.LatLong
tfLatLong locationGps = do
  let latLongLat = Beckn.OnDemand.Utils.Common.parseLatLong locationGps & (.lat)
  let latLongLon = Beckn.OnDemand.Utils.Common.parseLatLong locationGps & (.lon)
  pure $
    Kernel.External.Maps.LatLong
      { lat = latLongLat,
        lon = latLongLon
      }
