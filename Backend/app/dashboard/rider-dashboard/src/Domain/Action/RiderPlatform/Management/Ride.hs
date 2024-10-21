{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.RiderPlatform.Management.Ride
  ( getRideList,
    getRideRideinfo,
    getRideInfo,
    getRideRideInfo,
    getRideTripRoute,
    getRidePickupRoute,
    postRideSyncMultiple,
    postRideCancelMultiple,
    getRideKaptureList,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified Dashboard.Common
import qualified Dashboard.Common.Ride
import qualified Dashboard.RiderPlatform.Management.Ride
import qualified Dashboard.RiderPlatform.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations
import qualified SharedLogic.Transaction
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

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getRideInfo merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideInfo) rideId

getRideRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes)
getRideRideInfo merchantShortId opCity apiTokenInfo rideShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideRideInfo) rideShortId

getRideTripRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.Flow Kernel.External.Maps.GetRoutesResp)
getRideTripRoute merchantShortId opCity apiTokenInfo rideId lat lon = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideTripRoute) rideId lat lon

getRidePickupRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.Flow Kernel.External.Maps.GetRoutesResp)
getRidePickupRoute merchantShortId opCity apiTokenInfo rideId lat lon = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRidePickupRoute) rideId lat lon

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideCancelMultiple merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.RiderManagementAPI (API.Types.RiderPlatform.Management.RideAPI API.Types.RiderPlatform.Management.Ride.PostRideCancelMultipleEndpoint)) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.postRideCancelMultiple) req)

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.Flow Dashboard.Common.Ride.MultipleRideSyncResp)
postRideSyncMultiple merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.RiderManagementAPI (API.Types.RiderPlatform.Management.RideAPI API.Types.RiderPlatform.Management.Ride.PostRideSyncMultipleEndpoint)) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.postRideSyncMultiple) req)

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId Dashboard.Common.Ride)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.RiderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.rideDSL.getRideKaptureList) rideShortId countryCode phoneNumber supportPhoneNumber
