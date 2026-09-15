{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverFyEarnings where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data DriverFyEarningsResp = DriverFyEarningsResp
  { financialYear :: Kernel.Prelude.Int,
    quarters :: [FyQuarterEarningsEntity],
    totalNetEarnings :: Kernel.Types.Common.HighPrecMoney,
    totalTdsDeducted :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FyQuarterEarningsEntity = FyQuarterEarningsEntity {netEarnings :: Kernel.Types.Common.HighPrecMoney, quarter :: Kernel.Prelude.Int, tdsDeducted :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
