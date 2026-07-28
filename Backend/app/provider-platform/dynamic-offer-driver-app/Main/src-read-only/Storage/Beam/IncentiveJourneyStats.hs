{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IncentiveJourneyStats where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.IncentiveJourneyMilestone
import qualified Domain.Types.IncentiveJourneyStats
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IncentiveJourneyStatsT f = IncentiveJourneyStatsT
  { conditionOperator :: (B.C f (Kernel.Prelude.Maybe Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator)),
    conditionType :: (B.C f Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType),
    conditionValue :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currentValue :: (B.C f Kernel.Prelude.Int),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    journeyId :: (B.C f Kernel.Prelude.Text),
    milestoneId :: (B.C f Kernel.Prelude.Text),
    periodKey :: (B.C f Kernel.Prelude.Text),
    rewardType :: (B.C f Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType),
    rewardValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    status :: (B.C f Domain.Types.IncentiveJourneyStats.JourneyMilestoneStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyStatsT where
  data PrimaryKey IncentiveJourneyStatsT f = IncentiveJourneyStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyStatsId . id

type IncentiveJourneyStats = IncentiveJourneyStatsT Identity

$(enableKVPG (''IncentiveJourneyStatsT) [('id)] [[('driverId)]])

$(mkTableInstances (''IncentiveJourneyStatsT) "incentive_journey_stats")
