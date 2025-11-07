{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.Management.Ride
  ( getRideList,
    getRideInfo,
    getShareRideInfo,
    getShareRideInfoByShortId,
    getRideTripRoute,
    getRidePickupRoute,
    postRideSyncMultiple,
    postRideCancelMultiple,
    getRideKaptureList,
    cancellationChargesWaiveOff,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Dashboard.Common.Ride
import qualified Domain.Action.Dashboard.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getRideList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe API.Types.RiderPlatform.Management.Ride.BookingStatus ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow API.Types.RiderPlatform.Management.Ride.RideListRes
getRideList merchantShortId opCity _apiTokenInfo limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideList) limit offset bookingStatus rideShortId customerPhoneNo driverPhoneNo from to

getRideInfo :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.RideInfoRes
getRideInfo merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideInfo) rideId

rideInfoHitsCountKey :: Text -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> rideId <> ":hitsCount"

getShareRideInfo :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes
getShareRideInfo merchantShortId opCity rideId = do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey $ Kernel.Types.Id.getId rideId) shareRideApiRateLimitOptions
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getShareRideInfo) rideId

getShareRideInfoByShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.ShareRideInfoRes
getShareRideInfoByShortId merchantShortId opCity rideShortId = do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey $ Kernel.Types.Id.getShortId rideShortId) shareRideApiRateLimitOptions
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getShareRideInfoByShortId) rideShortId

getRideTripRoute :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.Flow Kernel.External.Maps.GetRoutesResp
getRideTripRoute merchantShortId opCity rideId lat lon = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideTripRoute) rideId lat lon

getRidePickupRoute :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Environment.Flow Kernel.External.Maps.GetRoutesResp
getRidePickupRoute merchantShortId opCity rideId lat lon = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getRidePickupRoute) rideId lat lon

postRideSyncMultiple :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.Flow Dashboard.Common.Ride.MultipleRideSyncResp
postRideSyncMultiple merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Domain.Action.Dashboard.Ride.validateMultipleRideSyncReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withResponseTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideSyncMultiple) req

postRideCancelMultiple :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postRideCancelMultiple merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideCancelMultiple) req

getRideKaptureList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow API.Types.RiderPlatform.Management.Ride.TicketRideListRes
getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideKaptureList) rideShortId countryCode phoneNumber supportPhoneNumber

cancellationChargesWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.Flow API.Types.RiderPlatform.Management.Ride.CancellationChargesWaiveOffRes)
cancellationChargesWaiveOff merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rideDSL.cancellationChargesWaiveOff) rideId
