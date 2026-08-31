{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats

data IncentiveJourneyStatsT f = IncentiveJourneyStatsT
  { conditionOperator :: B.C f (Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator),
    conditionType :: B.C f Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currentValue :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    journeyId :: B.C f Kernel.Prelude.Text,
    milestoneId :: B.C f Kernel.Prelude.Text,
    periodKey :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    rewardType :: B.C f Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    status :: B.C f Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyStatsT where
  data PrimaryKey IncentiveJourneyStatsT f = IncentiveJourneyStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyStatsId . id

type IncentiveJourneyStats = IncentiveJourneyStatsT Identity

$(enableKVPG ''IncentiveJourneyStatsT ['id] [['personId]])

$(mkTableInstancesGenericSchema ''IncentiveJourneyStatsT "incentive_journey_stats")
