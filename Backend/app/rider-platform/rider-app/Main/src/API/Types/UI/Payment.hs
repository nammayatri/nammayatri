{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Payment where

import Data.OpenApi (ToSchema)
import Kernel.Prelude
import Servant

data WalletRechargeReq = WalletRechargeReq
  { pointsAmount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
