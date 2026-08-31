{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Tools.Beam.UtilsTH

data CohortJourneyMapping = CohortJourneyMapping
  { cohortId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    startDate :: Kernel.Prelude.UTCTime,
    streakEndRewardExpirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakEndRewardType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    streakEndRewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    streakRange :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
