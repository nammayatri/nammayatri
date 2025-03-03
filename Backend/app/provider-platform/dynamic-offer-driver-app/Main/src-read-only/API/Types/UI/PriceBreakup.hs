{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PriceBreakup where

import qualified API.Types.UI.DriverOnboardingV2
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data MeterRidePriceRes = MeterRidePriceRes {breakup :: [API.Types.UI.DriverOnboardingV2.RateCardItem], fare :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
