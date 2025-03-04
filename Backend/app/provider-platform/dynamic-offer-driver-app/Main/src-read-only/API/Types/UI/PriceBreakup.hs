{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PriceBreakup where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data MeterRidePriceRes = MeterRidePriceRes {fare :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
