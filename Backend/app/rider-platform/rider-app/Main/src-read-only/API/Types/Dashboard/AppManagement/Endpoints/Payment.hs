{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Payment where

import qualified "this" API.Types.UI.RidePayment
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.FareBreakup
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RefundRequest
import qualified "this" Domain.Types.Ride
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import qualified "payment" Lib.Payment.Domain.Types.Refunds
import Servant
import Servant.Client

data RefundComponentItem = RefundComponentItem {component :: Domain.Types.FareBreakup.FareComponent, amount :: Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundRequestInfoResp = RefundRequestInfoResp
  { orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    requestedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    requestedRefundComponents :: Kernel.Prelude.Maybe [RefundComponentItem],
    approvedRefundComponents :: Kernel.Prelude.Maybe [RefundComponentItem],
    transactionAmount :: Kernel.Types.Common.PriceAPIEntity,
    refundPurpose :: Domain.Types.RefundRequest.RefundPurpose,
    status :: Domain.Types.RefundRequest.RefundRequestStatus,
    code :: Domain.Types.RefundRequest.RefundRequestCode,
    description :: Kernel.Prelude.Text,
    responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundsId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds),
    refundsAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    refundsTries :: Kernel.Prelude.Int,
    evidence :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundStatus :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.RefundStatus,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundRequestInitiateReq = RefundRequestInitiateReq {refundComponents :: [RefundComponentItem], description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, deductFromDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RefundRequestInitiateReq where
  hideSecrets = Kernel.Prelude.identity

data RefundRequestItem = RefundRequestItem
  { id :: Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest,
    orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    requestedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    transactionAmount :: Kernel.Types.Common.PriceAPIEntity,
    refundPurpose :: Domain.Types.RefundRequest.RefundPurpose,
    status :: Domain.Types.RefundRequest.RefundRequestStatus,
    code :: Domain.Types.RefundRequest.RefundRequestCode,
    description :: Kernel.Prelude.Text,
    responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundsId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds),
    refundsAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    refundsTries :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundRequestResp = RefundRequestResp {summary :: Dashboard.Common.Summary, refundRequests :: [RefundRequestItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundRequestRespondReq = RefundRequestRespondReq
  { approve :: Kernel.Prelude.Bool,
    responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundComponents :: Kernel.Prelude.Maybe [RefundComponentItem],
    retryRefunds :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    deductFromDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RefundRequestRespondReq where
  hideSecrets = Kernel.Prelude.identity

data RefundRequestRespondResp = RefundRequestRespondResp
  { status :: Domain.Types.RefundRequest.RefundRequestStatus,
    refundStatus :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.RefundStatus,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RefundRequestRespondResp where
  hideSecrets = Kernel.Prelude.identity

type API = ("payment" :> (GetPaymentRefundRequestList :<|> GetPaymentRefundRequestInfo :<|> PostPaymentRefundRequestRespond :<|> PostPaymentRefundRequestInitiate :<|> GetPaymentFareBreakup))

type GetPaymentRefundRequestList =
  ( "refundRequest" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.RefundRequest.RefundRequestStatus
      :> QueryParam "code" Domain.Types.RefundRequest.RefundRequestCode
      :> QueryParam
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "orderId"
           (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder)
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           RefundRequestResp
  )

type GetPaymentRefundRequestInfo =
  ( "refundRequest" :> Capture "refundRequestId" (Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest) :> "info"
      :> QueryParam
           "refreshRefunds"
           Kernel.Prelude.Bool
      :> Get '[JSON] RefundRequestInfoResp
  )

type PostPaymentRefundRequestRespond =
  ( "refundRequest" :> Capture "refundRequestId" (Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest) :> "respond"
      :> ReqBody
           '[JSON]
           RefundRequestRespondReq
      :> Post '[JSON] RefundRequestRespondResp
  )

type PostPaymentRefundRequestInitiate =
  ( "refundRequest" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "initiate" :> ReqBody '[JSON] RefundRequestInitiateReq
      :> Post
           '[JSON]
           RefundRequestRespondResp
  )

type GetPaymentFareBreakup = (Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "fareBreakup" :> Get '[JSON] API.Types.UI.RidePayment.FareBreakupRes)

data PaymentAPIs = PaymentAPIs
  { getPaymentRefundRequestList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient RefundRequestResp,
    getPaymentRefundRequestInfo :: Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient RefundRequestInfoResp,
    postPaymentRefundRequestRespond :: Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> RefundRequestRespondReq -> EulerHS.Types.EulerClient RefundRequestRespondResp,
    postPaymentRefundRequestInitiate :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> RefundRequestInitiateReq -> EulerHS.Types.EulerClient RefundRequestRespondResp,
    getPaymentFareBreakup :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> EulerHS.Types.EulerClient API.Types.UI.RidePayment.FareBreakupRes
  }

mkPaymentAPIs :: (Client EulerHS.Types.EulerClient API -> PaymentAPIs)
mkPaymentAPIs paymentClient = (PaymentAPIs {..})
  where
    getPaymentRefundRequestList :<|> getPaymentRefundRequestInfo :<|> postPaymentRefundRequestRespond :<|> postPaymentRefundRequestInitiate :<|> getPaymentFareBreakup = paymentClient

data PaymentUserActionType
  = GET_PAYMENT_REFUND_REQUEST_LIST
  | GET_PAYMENT_REFUND_REQUEST_INFO
  | POST_PAYMENT_REFUND_REQUEST_RESPOND
  | POST_PAYMENT_REFUND_REQUEST_INITIATE
  | GET_PAYMENT_FARE_BREAKUP
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PaymentUserActionType])
