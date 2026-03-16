module API.Types.UI.Payment where

import Data.OpenApi (ToSchema)
import Kernel.Prelude

data WalletRechargeReq = WalletRechargeReq
  { pointsAmount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
