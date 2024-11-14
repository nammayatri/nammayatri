module Domain.Action.ProviderPlatform.Management.Ride
  ( getRideList,
    postRideEndMultiple,
    postRideCancelMultiple,
    getRideInfo,
    postRideSync,
    postRideSyncMultiple,
    postRideRoute,
    getRideKaptureList,
    getRideFareBreakUp,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

buildManagementServerTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Ride) ->
  Maybe request ->
  m DT.Transaction
buildManagementServerTransaction apiTokenInfo =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing

getRideList ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Common.BookingStatus ->
  Maybe Currency ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ShortId Common.Ride) ->
  Maybe UTCTime ->
  Flow Common.RideListRes
getRideList merchantShortId opCity apiTokenInfo bookingStatus currency customerPhoneNo driverPhoneNo fareDiff from limit offset rideShortId to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideList) bookingStatus currency customerPhoneNo driverPhoneNo fareDiff from limit offset rideShortId to

postRideEndMultiple :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideEndReq -> Flow Common.MultipleRideEndResp
postRideEndMultiple merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMultipleRideEndReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideEndMultiple) req

postRideCancelMultiple :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideCancelReq -> Flow Common.MultipleRideCancelResp
postRideCancelMultiple merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMultipleRideCancelReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideCancelMultiple) req

getRideInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Flow Common.RideInfoRes
getRideInfo merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideInfo) rideId

postRideSync :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Flow Common.RideSyncRes
postRideSync merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction apiTokenInfo (Just rideId) T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideSync) rideId

postRideSyncMultiple :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideSyncReq -> Flow Common.MultipleRideSyncRes
postRideSyncMultiple merchantShortId opCity apiTokenInfo rideSyncReq = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction apiTokenInfo Nothing (Just rideSyncReq)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideSyncMultiple) rideSyncReq

postRideRoute ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Ride ->
  Flow Common.RideRouteRes
postRideRoute merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideRoute) rideId

getRideKaptureList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.TicketRideListRes
getRideKaptureList merchantShortId opCity apiTokenInfo mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction apiTokenInfo Nothing T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideKaptureList) mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber

getRideFareBreakUp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Flow Common.FareBreakUpRes
getRideFareBreakUp merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideFareBreakUp) rideId
