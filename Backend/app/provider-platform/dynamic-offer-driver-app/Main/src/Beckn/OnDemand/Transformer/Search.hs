{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified Beckn.Types.Core.Taxi.API.Search
import qualified Beckn.Types.Core.Taxi.Common.Address
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.List as L
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
import Kernel.Utils.Common (fromMaybeM, type (:::))
import Tools.Error

buildSearchReq :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.SearchReqMessage -> BecknV2.OnDemand.Types.Context -> m Domain.Action.Beckn.Search.DSearchReq
buildSearchReq messageId subscriber req context = do
  let bapId_ = subscriber.subscriber_id
  let bapUri_ = subscriber.subscriber_url
  customerLanguage_ <- Beckn.OnDemand.Utils.Search.buildCustomerLanguage req
  customerPhoneNum_ <- Beckn.OnDemand.Utils.Search.buildCustomerPhoneNumber req
  let device_ = Nothing
  disabilityTag_ <- Beckn.OnDemand.Utils.Search.buildDisabilityTag req
  isReallocationEnabled_ <- Beckn.OnDemand.Utils.Search.getIsReallocationEnabled req
  multipleRoutes <- Beckn.OnDemand.Utils.Search.buildMultipleRoutesTag req
  let messageId_ = messageId
  now <- Kernel.Types.Common.getCurrentTime
  routeDistance_ <- Beckn.OnDemand.Utils.Search.getDistance req
  routeDuration_ <- Beckn.OnDemand.Utils.Search.getDuration req
  routePoints_ <- Beckn.OnDemand.Utils.Search.buildRoutePoints req
  bapCountry_ <- Beckn.OnDemand.Utils.Common.getContextCountry context
  dropAddrress_ <- Beckn.OnDemand.Utils.Search.getDropOffLocation req & tfAddress
  dropLocation_ <- tfLatLong `mapM` Beckn.OnDemand.Utils.Search.getDropOffLocationGps req
  pickupAddress_ <- Beckn.OnDemand.Utils.Search.getPickUpLocation req >>= (tfAddress . Just)
  pickupLocation_ <- Beckn.OnDemand.Utils.Search.getPickUpLocationGps req >>= tfLatLong
  let pickupTime_ = fromMaybe now $ Beckn.OnDemand.Utils.Search.getPickUpTime req
  transactionId_ <- Beckn.OnDemand.Utils.Common.getTransactionId context
  pure $
    Domain.Action.Beckn.Search.DSearchReq
      { bapCountry = bapCountry_,
        bapId = bapId_,
        bapUri = bapUri_,
        customerLanguage = customerLanguage_,
        customerPhoneNum = customerPhoneNum_,
        device = device_,
        disabilityTag = disabilityTag_,
        dropAddrress = dropAddrress_,
        dropLocation = dropLocation_,
        isReallocationEnabled = isReallocationEnabled_,
        messageId = messageId_,
        pickupAddress = pickupAddress_,
        pickupLocation = pickupLocation_,
        pickupTime = pickupTime_,
        routeDistance = routeDistance_,
        routeDuration = routeDuration_,
        routePoints = routePoints_,
        transactionId = transactionId_,
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
