{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Tools.Beam.UtilsTH

data IncentiveJourneyStats = IncentiveJourneyStats
  { conditionOperator :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator,
    conditionType :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currentValue :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats,
    journeyId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney,
    milestoneId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone,
    periodKey :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person,
    rewardType :: Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data JourneyMilestoneStatus = NotStarted | InProgress | Completed | Rewarded | WaivedOff deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyMilestoneStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''JourneyMilestoneStatus)
