{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.MultiModal
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.MultiModal
import qualified Domain.Action.Dashboard.RideBooking.MultiModal
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "this" Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("multiModal" :> (GetMultiModalList :<|> PostMultiModalSendMessage :<|> PostMultiModalSendDirectMessage :<|> PostMultiModalAddComment :<|> GetMultiModalGetComments))

type GetMultiModalList = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/MULTI_MODAL/GET_MULTI_MODAL_LIST" :> API.Types.Dashboard.RideBooking.MultiModal.GetMultiModalList)

type PostMultiModalSendMessage =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_SEND_MESSAGE"
      :> API.Types.Dashboard.RideBooking.MultiModal.PostMultiModalSendMessage
  )

type PostMultiModalSendDirectMessage =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_SEND_DIRECT_MESSAGE"
      :> API.Types.Dashboard.RideBooking.MultiModal.PostMultiModalSendDirectMessage
  )

type PostMultiModalAddComment = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_ADD_COMMENT" :> API.Types.Dashboard.RideBooking.MultiModal.PostMultiModalAddComment)

type GetMultiModalGetComments = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/MULTI_MODAL/GET_MULTI_MODAL_GET_COMMENTS" :> API.Types.Dashboard.RideBooking.MultiModal.GetMultiModalGetComments)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMultiModalList merchantId city :<|> postMultiModalSendMessage merchantId city :<|> postMultiModalSendDirectMessage merchantId city :<|> postMultiModalAddComment merchantId city :<|> getMultiModalGetComments merchantId city

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe ([Domain.Types.BookingStatus.BookingStatus]) -> Kernel.Prelude.Maybe ([Domain.Types.Journey.JourneyStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.Booking.API.BookingRequestType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList a17 a16 _a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MultiModal.getMultiModalList a17 a16 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postMultiModalSendMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.MultiModal.CustomerSendMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMultiModalSendMessage a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MultiModal.postMultiModalSendMessage a5 a4 a2 a1

postMultiModalSendDirectMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.RideBooking.MultiModal.CustomerSendDirectMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMultiModalSendDirectMessage a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MultiModal.postMultiModalSendDirectMessage a4 a3 a1

postMultiModalAddComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.MultiModal.CustomerCommentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMultiModalAddComment a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MultiModal.postMultiModalAddComment a5 a4 a2 a1

getMultiModalGetComments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.MultiModal.CustomerCommentsResp)
getMultiModalGetComments a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.MultiModal.getMultiModalGetComments a4 a3 a1
