{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.IncentiveJourney where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats
import Servant
import Tools.Auth

data IncentiveJourneyHistoryItem = IncentiveJourneyHistoryItem
  { areaType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneAreaType,
    completedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    conditionOperator :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    currentValue :: Kernel.Prelude.Int,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyName :: Data.Text.Text,
    journeyType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourneyType,
    milestoneDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    milestoneId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    milestoneName :: Kernel.Prelude.Maybe Data.Text.Text,
    milestoneOrder :: Kernel.Prelude.Int,
    periodKey :: Data.Text.Text,
    rewardType :: Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    specialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    status :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyHistoryRes = IncentiveJourneyHistoryRes {history :: [IncentiveJourneyHistoryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListItem = IncentiveJourneyListItem
  { description :: Kernel.Prelude.Maybe Data.Text.Text,
    enabled :: Kernel.Prelude.Bool,
    endDate :: Kernel.Prelude.UTCTime,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourneyType,
    milestones :: [IncentiveJourneyMilestoneItem],
    name :: Data.Text.Text,
    startDate :: Kernel.Prelude.UTCTime,
    streakRange :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListRes = IncentiveJourneyListRes {journeys :: [IncentiveJourneyListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneItem = IncentiveJourneyMilestoneItem
  { areaType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneAreaType,
    conditionOperator :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    currentValue :: Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    milestoneId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    name :: Kernel.Prelude.Maybe Data.Text.Text,
    order :: Kernel.Prelude.Int,
    rewardType :: Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    specialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    status :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
