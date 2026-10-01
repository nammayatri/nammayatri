{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Payout where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import Servant.Client

data RetriggerPayoutReq = RetriggerPayoutReq {orderIds :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RetriggerPayoutReq where
  hideSecrets = Kernel.Prelude.identity

data RetriggerPayoutResp = RetriggerPayoutResp {results :: [RetriggerPayoutResult]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RetriggerPayoutResult = RetriggerPayoutResult
  { orderId :: Kernel.Prelude.Text,
    customerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: RetriggerPayoutStatus,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RetriggerPayoutStatus
  = RESENT
  | TRIGGERED
  | ALREADY_TRIGGERED
  | ALREADY_RETRIGGERED
  | ORDER_NOT_FOUND
  | NOT_E09
  | UNSUPPORTED_TYPE
  | STATUS_CHECK_FAILED
  | NO_VPA
  | NOTHING_PENDING
  | FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("payout" :> (GetPayoutPayoutOrderHelper :<|> PostPayoutPayoutRetriggerHelper))

type GetPayoutPayoutOrder = ("payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutOrderResp)

type GetPayoutPayoutOrderHelper =
  ( "payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutOrderResp
  )

type PostPayoutPayoutRetrigger = ("payout" :> "retrigger" :> ReqBody '[JSON] RetriggerPayoutReq :> Post '[JSON] RetriggerPayoutResp)

type PostPayoutPayoutRetriggerHelper = ("payout" :> "retrigger" :> QueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] RetriggerPayoutReq :> Post '[JSON] RetriggerPayoutResp)

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutOrder :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutOrderResp,
    postPayoutPayoutRetrigger :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> RetriggerPayoutReq -> EulerHS.Types.EulerClient RetriggerPayoutResp
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutOrder :<|> postPayoutPayoutRetrigger = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_ORDER
  | POST_PAYOUT_PAYOUT_RETRIGGER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PayoutUserActionType])
