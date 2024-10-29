{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FareCalculator where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data EstimateApi = EstimateApi
  { createdAt :: Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    maxFare :: Kernel.Types.Common.HighPrecMoney,
    minFare :: Kernel.Types.Common.HighPrecMoney,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareResponse = FareResponse {estimatedFares :: [EstimateApi]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
