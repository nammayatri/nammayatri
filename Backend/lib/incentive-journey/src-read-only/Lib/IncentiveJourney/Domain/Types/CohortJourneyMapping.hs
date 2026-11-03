{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Tools.Beam.UtilsTH

data CohortJourneyMapping = CohortJourneyMapping
  { allowCancellationByDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssign :: Kernel.Prelude.Maybe Data.Aeson.Value,
    cohortId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping,
    isMg :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    maxWaiveOffCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity,
    mgAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    parentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping),
    poolingPriority :: Kernel.Prelude.Maybe Data.Aeson.Value,
    startDate :: Kernel.Prelude.UTCTime,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardMetadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    streakEndRewardType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakRange :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
