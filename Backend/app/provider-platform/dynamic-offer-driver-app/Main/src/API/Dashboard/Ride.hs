{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Ride where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (Money, logTagInfo, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import Servant hiding (Unauthorized, throwError)
import SharedLogic.Merchant (findMerchantByShortId)

type API =
  "ride"
    :> ( Common.RideListAPI
           :<|> Common.RideStartAPI
           :<|> Common.RideEndAPI
           :<|> Common.MultipleRideEndAPI
           :<|> Common.CurrentActiveRideAPI
           :<|> Common.RideCancelAPI
           :<|> Common.MultipleRideCancelAPI
           :<|> Common.RideInfoAPI
           :<|> Common.RideSyncAPI
           :<|> Common.MultipleRideSyncAPI
           :<|> Common.RideRouteAPI
           :<|> Common.BookingWithVehicleNumberAndPhoneAPI
           :<|> Common.TicketRideListAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> multipleRideEnd merchantId
    :<|> currentActiveRide merchantId
    :<|> rideCancel merchantId
    :<|> multipleRideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId
    :<|> multipleRideSync merchantId
    :<|> rideRoute merchantId
    :<|> bookingWithVehicleNumberAndPhone merchantId
    :<|> ticketRideList merchantId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Money ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbFareDiff mbfrom mbto =
  withFlowHandlerAPI . DRide.rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbFareDiff mbfrom mbto

rideStart :: ShortId DM.Merchant -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId reqRideId Common.StartRideReq {point} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  let dashboardReq = SHandler.DashboardStartRideReq {point, merchantId}
  shandle <- SHandler.buildStartRideHandle merchantId
  SHandler.dashboardStartRide shandle rideId dashboardReq

rideEnd :: ShortId DM.Merchant -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId reqRideId Common.EndRideReq {point} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let merchantId = merchant.id
  let dashboardReq = EHandler.DashboardEndRideReq {point, merchantId}
  shandle <- EHandler.buildEndRideHandle merchantId
  EHandler.dashboardEndRide shandle rideId dashboardReq

multipleRideEnd :: ShortId DM.Merchant -> Common.MultipleRideEndReq -> FlowHandler Common.MultipleRideEndResp
multipleRideEnd merchantShortId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideEndReq req
  merchant <- findMerchantByShortId merchantShortId
  shandle <- EHandler.buildEndRideHandle merchant.id
  logTagInfo "dashboard -> multipleRideEnd : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let cronJobReq =
            EHandler.CronJobEndRideReq
              { point = reqItem.point,
                merchantId = merchant.id
              }
      Success <- EHandler.cronJobEndRide shandle rideId cronJobReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

currentActiveRide :: ShortId DM.Merchant -> Text -> FlowHandler (Id Common.Ride)
currentActiveRide merchantShortId vehicleNumber = withFlowHandlerAPI $ DRide.currentActiveRide merchantShortId vehicleNumber

rideCancel :: ShortId DM.Merchant -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId reqRideId Common.CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  let dashboardReq =
        CHandler.CancelRideReq
          { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reasonCode,
            additionalInfo
          }
  CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id rideId dashboardReq

multipleRideCancel :: ShortId DM.Merchant -> Common.MultipleRideCancelReq -> FlowHandler Common.MultipleRideCancelResp
multipleRideCancel merchantShortId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideCancelReq req
  merchant <- findMerchantByShortId merchantShortId
  logTagInfo "dashboard -> multipleRideCancel : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let dashboardReq =
            CHandler.CancelRideReq
              { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reqItem.reasonCode,
                additionalInfo = reqItem.additionalInfo
              }
      Success <- CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id rideId dashboardReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId = withFlowHandlerAPI . DRide.rideInfo merchantShortId

rideSync :: ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId = withFlowHandlerAPI . DRide.rideSync merchantShortId

multipleRideSync :: ShortId DM.Merchant -> Common.MultipleRideSyncReq -> FlowHandler Common.MultipleRideSyncRes
multipleRideSync merchantShortId = withFlowHandlerAPI . DRide.multipleRideSync merchantShortId

rideRoute :: ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideRouteRes
rideRoute merchantShortId rideId = withFlowHandlerAPI $ DRide.rideRoute merchantShortId rideId

bookingWithVehicleNumberAndPhone :: ShortId DM.Merchant -> Common.BookingWithVehicleAndPhoneReq -> FlowHandler Common.BookingWithVehicleAndPhoneRes
bookingWithVehicleNumberAndPhone merchantShortId = withFlowHandlerAPI . DRide.bookingWithVehicleNumberAndPhone merchantShortId

ticketRideList :: ShortId DM.Merchant -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.TicketRideListRes
ticketRideList merchantShortId mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = withFlowHandlerAPI $ DRide.ticketRideList merchantShortId mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber
