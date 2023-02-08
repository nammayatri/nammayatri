module API.Dashboard.Ride where

import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Common
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
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import SharedLogic.Transporter (findMerchantByShortId)

type API =
  "ride"
    :> ( Common.RideListAPI
           :<|> Common.RideStartAPI
           :<|> Common.RideEndAPI
           :<|> Common.RideCancelAPI
           :<|> Common.RideInfoAPI
           :<|> Common.RideSyncAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> rideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone =
  withFlowHandlerAPI . DRide.rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone

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
  shandle <- EHandler.buildEndRideHandle merchantId rideId
  EHandler.dashboardEndRide shandle rideId dashboardReq

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

rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId = withFlowHandlerAPI . DRide.rideInfo merchantShortId

rideSync :: ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId = withFlowHandlerAPI . DRide.rideSync merchantShortId
