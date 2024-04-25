{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Estimate where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Estimate = Estimate
  { createdAt :: Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    fareParams :: Kernel.Prelude.Maybe Domain.Types.FareParameters.FareParameters,
    farePolicy :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicy,
    id :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    maxFare :: Kernel.Types.Common.Money,
    minFare :: Kernel.Types.Common.Money,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show)
