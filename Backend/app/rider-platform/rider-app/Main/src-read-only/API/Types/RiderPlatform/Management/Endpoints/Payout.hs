{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Payout where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import Servant.Client

data TransferResolveAction
  = RECHECK
  | MARK_SUCCEEDED
  | MARK_FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferResolveReq = TransferResolveReq {action :: TransferResolveAction, reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TransferResolveReq where
  hideSecrets = Kernel.Prelude.identity

data TransferResolveRes = TransferResolveRes {transferId :: Kernel.Prelude.Text, status :: Kernel.Prelude.Text, message :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferReviewItem = TransferReviewItem
  { transferId :: Kernel.Prelude.Text,
    direction :: Kernel.Prelude.Text,
    railKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Text,
    railOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    railStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    feeAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    ownerType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ownerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    attemptCount :: Kernel.Prelude.Int,
    holdEntryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entryIds :: [Kernel.Prelude.Text],
    coveredEntryIds :: [Kernel.Prelude.Text],
    canResolve :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferReviewListRes = TransferReviewListRes {items :: [TransferReviewItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("payout" :> (GetPayoutPayoutOrderHelper :<|> GetPayoutTransferNeedsReviewHelper :<|> PostPayoutTransferResolveHelper))

type GetPayoutPayoutOrder = ("payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> Get ('[JSON]) Lib.Payment.API.Payout.Types.PayoutOrderResp)

type GetPayoutPayoutOrderHelper =
  ( "payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           Lib.Payment.API.Payout.Types.PayoutOrderResp
  )

type GetPayoutTransferNeedsReview = ("transfer" :> "needsReview" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) TransferReviewListRes)

type GetPayoutTransferNeedsReviewHelper =
  ( "transfer" :> "needsReview" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get ('[JSON]) TransferReviewListRes
  )

type PostPayoutTransferResolve = ("transfer" :> Capture "transferId" Kernel.Prelude.Text :> "resolve" :> ReqBody ('[JSON]) TransferResolveReq :> Post ('[JSON]) TransferResolveRes)

type PostPayoutTransferResolveHelper =
  ( "transfer" :> Capture "transferId" Kernel.Prelude.Text :> "resolve" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           TransferResolveReq
      :> Post ('[JSON]) TransferResolveRes
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutOrder :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutOrderResp),
    getPayoutTransferNeedsReview :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient TransferReviewListRes),
    postPayoutTransferResolve :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> TransferResolveReq -> EulerHS.Types.EulerClient TransferResolveRes)
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutOrder :<|> getPayoutTransferNeedsReview :<|> postPayoutTransferResolve = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_ORDER
  | GET_PAYOUT_TRANSFER_NEEDS_REVIEW
  | POST_PAYOUT_TRANSFER_RESOLVE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PayoutUserActionType)])
