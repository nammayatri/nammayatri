{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.IncentiveJourney where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.IncentiveJourneyMilestone
import qualified Domain.Types.IncentiveJourneyStats
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Servant
import Tools.Auth

data IncentiveJourneyHistoryItem = IncentiveJourneyHistoryItem
  { completedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    conditionOperator :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    currentValue :: Kernel.Prelude.Int,
    dropSpecialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    journeyId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyName :: Data.Text.Text,
    journeyType :: Kernel.Prelude.Maybe Domain.Types.IncentiveJourney.IncentiveJourneyType,
    milestoneDescription :: Kernel.Prelude.Maybe Data.Text.Text,
    milestoneId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    milestoneOrder :: Kernel.Prelude.Int,
    periodKey :: Data.Text.Text,
    pickupSpecialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    rewardType :: Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus
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
    journeyId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney,
    journeyType :: Kernel.Prelude.Maybe Domain.Types.IncentiveJourney.IncentiveJourneyType,
    milestones :: [IncentiveJourneyMilestoneItem],
    name :: Data.Text.Text,
    startDate :: Kernel.Prelude.UTCTime,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListRes = IncentiveJourneyListRes {journeys :: [IncentiveJourneyListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneItem = IncentiveJourneyMilestoneItem
  { conditionOperator :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    currentValue :: Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    dropSpecialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    milestoneId :: Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    order :: Kernel.Prelude.Int,
    pickupSpecialLocationNames :: Kernel.Prelude.Maybe [Data.Text.Text],
    rewardType :: Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
