{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
    getRideListV2,
    postRideWaiverRideCancellationPenalty,
    getRideAgentList,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
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
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ShortId Common.Ride) ->
  Maybe UTCTime ->
  Flow Common.RideListRes
getRideList merchantShortId opCity apiTokenInfo bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideList) bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to

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

getRideListV2 :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Kernel.Types.Common.Currency) -> Maybe Text -> Maybe Text -> Maybe (Kernel.Prelude.UTCTime) -> Maybe Int -> Maybe Int -> Maybe (ShortId Common.Ride) -> Maybe Common.RideStatus -> Maybe (Kernel.Prelude.UTCTime) -> Flow Common.RideListResV2
getRideListV2 merchantShortId opCity apiTokenInfo currency customerPhoneNo driverPhoneNo from limit offset rideShortId rideStatus to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideListV2) currency customerPhoneNo driverPhoneNo from limit offset rideShortId rideStatus to

postRideWaiverRideCancellationPenalty :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Types.Id.Id Common.Ride -> Common.WaiverRideCancellationPenaltyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.postRideWaiverRideCancellationPenalty) rideId req

getRideAgentList :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Common.BookingStatus -> Maybe Currency -> Maybe Text -> Maybe Text -> Maybe Kernel.Prelude.UTCTime -> Maybe Int -> Maybe Int -> Maybe (ShortId Common.Ride) -> Maybe UTCTime -> Maybe Text -> Flow Common.RideListRes)
getRideAgentList merchantShortId opCity apiTokenInfo bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to vehicleNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.rideDSL.getRideAgentList) bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to vehicleNo
