{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.RiderPlatform.Management.Ride
  ( getRideList,
    getRideRideinfo,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getRideList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Prelude.Maybe (Dashboard.RiderPlatform.Ride.BookingStatus) ->
  Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Environment.Flow API.Types.RiderPlatform.Management.Ride.RideListRes
getRideList merchantShortId opCity apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideList) limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to

getRideRideinfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Ride ->
  Environment.Flow API.Types.RiderPlatform.Management.Ride.RideInfoRes
getRideRideinfo merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideRideinfo) rideId
