{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping where

import Data.Aeson
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data AutoApplyCohortMapping = AutoApplyCohortMapping
  { allowIfNoMapping :: Kernel.Prelude.Bool,
    cohortJourneyMappingId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping,
    createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping,
    merchantId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
