{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Payout where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data PayoutCancelReq = PayoutCancelReq {reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutCancelReq where
  hideSecrets = Kernel.Prelude.identity

data PayoutStatus
  = PENDING
  | PROCESSING
  | PROCESSED
  | FAILED
  | CANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutStatusResp = PayoutStatusResp
  { status :: PayoutStatus,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutStatusResp where
  hideSecrets = Kernel.Prelude.identity

type API = ("payout" :> (GetPayoutStatus :<|> PostPayoutCancel :<|> PostPayoutRetry))

type GetPayoutStatus = (Capture "rideId" Kernel.Prelude.Text :> "status" :> Get ('[JSON]) PayoutStatusResp)

type PostPayoutCancel = (Capture "rideId" Kernel.Prelude.Text :> "cancel" :> ReqBody ('[JSON]) PayoutCancelReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostPayoutRetry = (Capture "rideId" Kernel.Prelude.Text :> "retry" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data PayoutAPIs = PayoutAPIs
  { getPayoutStatus :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient PayoutStatusResp),
    postPayoutCancel :: (Kernel.Prelude.Text -> PayoutCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postPayoutRetry :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutStatus :<|> postPayoutCancel :<|> postPayoutRetry = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_STATUS
  | POST_PAYOUT_CANCEL
  | POST_PAYOUT_RETRY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PayoutUserActionType)])
