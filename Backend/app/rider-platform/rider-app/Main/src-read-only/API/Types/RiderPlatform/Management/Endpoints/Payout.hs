{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Payout where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import Servant.Client

type API = ("payout" :> GetPayoutPayoutOrder)

type GetPayoutPayoutOrder = ("payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> Get ('[JSON]) Lib.Payment.API.Payout.Types.PayoutOrderResp)

newtype PayoutAPIs = PayoutAPIs {getPayoutPayoutOrder :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutOrderResp)}

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutOrder = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_ORDER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON PayoutUserActionType where
  toJSON (GET_PAYOUT_PAYOUT_ORDER) = Data.Aeson.String "GET_PAYOUT_PAYOUT_ORDER"

instance FromJSON PayoutUserActionType where
  parseJSON (Data.Aeson.String "GET_PAYOUT_PAYOUT_ORDER") = pure GET_PAYOUT_PAYOUT_ORDER
  parseJSON _ = fail "GET_PAYOUT_PAYOUT_ORDER expected"

$(Data.Singletons.TH.genSingletons [(''PayoutUserActionType)])
