{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified Beckn.Types.Core.Taxi.API.Search
import qualified Beckn.Types.Core.Taxi.Common.Address
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
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

buildSearchReq :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.SearchReqMessage -> BecknV2.OnDemand.Types.Context -> m (Domain.Action.Beckn.Search.DSearchReq)
buildSearchReq messageId city subscriber req context = do
  let bapCity_ = city
  let bapId_ = subscriber.subscriber_id
  let bapUri_ = subscriber.subscriber_url
  let customerLanguage_ = Beckn.OnDemand.Utils.Search.buildCustomerLanguage req
  let customerPhoneNum_ = Beckn.OnDemand.Utils.Search.buildCustomerPhoneNumber req
  let device_ = Nothing
  let disabilityTag_ = Beckn.OnDemand.Utils.Search.buildDisabilityTag req
  let isReallocationEnabled_ = Beckn.OnDemand.Utils.Search.getIsReallocationEnabled req
  let messageId_ = messageId
  let routeDistance_ = Beckn.OnDemand.Utils.Search.getDistance req
  let routeDuration_ = Beckn.OnDemand.Utils.Search.getDuration req
  let routePoints_ = Beckn.OnDemand.Utils.Search.buildRoutePoints req
  bapCountry_ <- Beckn.OnDemand.Utils.Common.getContextCountry context
  dropAddrress_ <- Beckn.OnDemand.Utils.Search.getDropOffLocation req & tfAddress
  dropLocation_ <- Beckn.OnDemand.Utils.Search.getDropOffLocationGps req & tfLatLong
  pickupAddress_ <- Beckn.OnDemand.Utils.Search.getPickUpLocation req & tfAddress
  pickupLocation_ <- Beckn.OnDemand.Utils.Search.getPickUpLocationGps req & tfLatLong
  pickupTime_ <- Kernel.Types.Common.getCurrentTime
  transactionId_ <- Beckn.OnDemand.Utils.Common.getTransactionId context
  pure $ Domain.Action.Beckn.Search.DSearchReq {bapCity = bapCity_, bapCountry = bapCountry_, bapId = bapId_, bapUri = bapUri_, customerLanguage = customerLanguage_, customerPhoneNum = customerPhoneNum_, device = device_, disabilityTag = disabilityTag_, dropAddrress = dropAddrress_, dropLocation = dropLocation_, isReallocationEnabled = isReallocationEnabled_, messageId = messageId_, pickupAddress = pickupAddress_, pickupLocation = pickupLocation_, pickupTime = pickupTime_, routeDistance = routeDistance_, routeDuration = routeDuration_, routePoints = routePoints_, transactionId = transactionId_}

tfAddress :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => BecknV2.OnDemand.Types.Location -> m (Maybe Beckn.Types.Core.Taxi.Common.Address.Address)
tfAddress location = do
  let area_code_ = Nothing
  let building_ = Nothing
  let city_ = Nothing
  let country_ = Nothing
  let door_ = location.locationAddress
  let locality_ = Nothing
  let state_ = Nothing
  let street_ = Nothing
  let ward_ = Nothing
  let returnData = Beckn.Types.Core.Taxi.Common.Address.Address {area_code = area_code_, building = building_, city = city_, country = country_, door = door_, locality = locality_, state = state_, street = street_, ward = ward_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfLatLong :: (Kernel.Types.App.HasFlowEnv m r '["_version" ::: Data.Text.Text]) => Data.Text.Text -> m (Kernel.External.Maps.LatLong)
tfLatLong locationGps = do
  let lat_ = Beckn.OnDemand.Utils.Common.parseLatLong locationGps & Kernel.External.Maps.lat
  let lon_ = Beckn.OnDemand.Utils.Common.parseLatLong locationGps & Kernel.External.Maps.lon
  pure $ Kernel.External.Maps.LatLong {lat = lat_, lon = lon_}
