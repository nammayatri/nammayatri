{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Payout where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Servant.Client

type API = ("payout" :> (GetPayoutPayout :<|> PostPayoutPayoutRetry :<|> PostPayoutPayoutCancel :<|> PostPayoutPayoutCash :<|> PostPayoutPayoutVpaDelete :<|> PostPayoutPayoutVpaUpdate :<|> PostPayoutPayoutVpaRefundRegistration))

type GetPayoutPayout = ("payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> Get ('[JSON]) Lib.Payment.API.Payout.Types.PayoutRequestResp)

type PostPayoutPayoutRetry =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "retry"
      :> Post
           ('[JSON])
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCancel =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cancel"
      :> ReqBody
           ('[JSON])
           Lib.Payment.API.Payout.Types.PayoutCancelReq
      :> Post ('[JSON]) Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCash =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cash"
      :> ReqBody
           ('[JSON])
           Lib.Payment.API.Payout.Types.PayoutCashUpdateReq
      :> Post ('[JSON]) Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaDelete = ("payout" :> "vpa" :> "delete" :> ReqBody ('[JSON]) Lib.Payment.API.Payout.Types.DeleteVpaReq :> Post ('[JSON]) Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaUpdate = ("payout" :> "vpa" :> "update" :> ReqBody ('[JSON]) Lib.Payment.API.Payout.Types.UpdateVpaReq :> Post ('[JSON]) Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaRefundRegistration =
  ( "payout" :> "vpa" :> "refundRegistration" :> ReqBody ('[JSON]) Lib.Payment.API.Payout.Types.RefundRegAmountReq
      :> Post
           ('[JSON])
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayout :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutRequestResp),
    postPayoutPayoutRetry :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess),
    postPayoutPayoutCancel :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess),
    postPayoutPayoutCash :: (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess),
    postPayoutPayoutVpaDelete :: (Lib.Payment.API.Payout.Types.DeleteVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess),
    postPayoutPayoutVpaUpdate :: (Lib.Payment.API.Payout.Types.UpdateVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess),
    postPayoutPayoutVpaRefundRegistration :: (Lib.Payment.API.Payout.Types.RefundRegAmountReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess)
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayout :<|> postPayoutPayoutRetry :<|> postPayoutPayoutCancel :<|> postPayoutPayoutCash :<|> postPayoutPayoutVpaDelete :<|> postPayoutPayoutVpaUpdate :<|> postPayoutPayoutVpaRefundRegistration = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT
  | POST_PAYOUT_PAYOUT_RETRY
  | POST_PAYOUT_PAYOUT_CANCEL
  | POST_PAYOUT_PAYOUT_CASH
  | POST_PAYOUT_PAYOUT_VPA_DELETE
  | POST_PAYOUT_PAYOUT_VPA_UPDATE
  | POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PayoutUserActionType)])
