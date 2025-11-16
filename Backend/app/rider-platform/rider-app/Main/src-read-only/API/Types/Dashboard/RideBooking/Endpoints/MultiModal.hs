{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.MultiModal where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "this" Domain.Types.Journey
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CustomerCommentReq = CustomerCommentReq {body :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerCommentsResp = CustomerCommentsResp {comments :: Kernel.Prelude.Maybe [Kernel.Prelude.Text], customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerSendMessageReq = CustomerSendMessageReq {channel :: MediaChannel, message :: Kernel.Prelude.Text, title :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaChannel
  = WHATSAPP
  | PUSH_NOTIFICATION
  | SMS
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("multiModal" :> (GetMultiModalList :<|> PostMultiModalSendMessage :<|> PostMultiModalAddComment :<|> GetMultiModalGetComments))

type GetMultiModalList =
  ( "list" :> QueryParam "limit" EulerHS.Prelude.Integer :> QueryParam "offset" EulerHS.Prelude.Integer
      :> QueryParam
           "bookingOffset"
           EulerHS.Prelude.Integer
      :> QueryParam "journeyOffset" EulerHS.Prelude.Integer
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
      :> QueryParam
           "email"
           Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           EulerHS.Prelude.Integer
      :> QueryParam
           "toDate"
           EulerHS.Prelude.Integer
      :> QueryParam
           "rideStatus"
           [Domain.Types.BookingStatus.BookingStatus]
      :> QueryParam
           "journeyStatus"
           [Domain.Types.Journey.JourneyStatus]
      :> QueryParam
           "isPaymentSuccess"
           Kernel.Prelude.Bool
      :> QueryParam
           "bookingRequestType"
           Domain.Types.Booking.API.BookingRequestType
      :> QueryParam
           "customerId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListResV2
  )

type PostMultiModalSendMessage = ("sendMessage" :> Capture "customerId" Kernel.Prelude.Text :> ReqBody '[JSON] CustomerSendMessageReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMultiModalAddComment = ("addComment" :> Capture "customerId" Kernel.Prelude.Text :> ReqBody '[JSON] CustomerCommentReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMultiModalGetComments = ("getComments" :> Capture "customerId" Kernel.Prelude.Text :> Get '[JSON] CustomerCommentsResp)

data MultiModalAPIs = MultiModalAPIs
  { getMultiModalList :: Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Domain.Action.UI.Booking.BookingListResV2,
    postMultiModalSendMessage :: Kernel.Prelude.Text -> CustomerSendMessageReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMultiModalAddComment :: Kernel.Prelude.Text -> CustomerCommentReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMultiModalGetComments :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient CustomerCommentsResp
  }

mkMultiModalAPIs :: (Client EulerHS.Types.EulerClient API -> MultiModalAPIs)
mkMultiModalAPIs multiModalClient = (MultiModalAPIs {..})
  where
    getMultiModalList :<|> postMultiModalSendMessage :<|> postMultiModalAddComment :<|> getMultiModalGetComments = multiModalClient

data MultiModalUserActionType
  = GET_MULTI_MODAL_LIST
  | POST_MULTI_MODAL_SEND_MESSAGE
  | POST_MULTI_MODAL_ADD_COMMENT
  | GET_MULTI_MODAL_GET_COMMENTS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MultiModalUserActionType])
