{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UnifiedDashboard.RideBooking.HealthCheck (getHealthCheckTest) where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest _merchantShortId _opCity = do error "Logic yet to be decided"
