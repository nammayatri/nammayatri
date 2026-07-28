{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IncentiveJourneyMilestone where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.IncentiveJourneyMilestone
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IncentiveJourneyMilestoneT f = IncentiveJourneyMilestoneT
  { conditionOperator :: (B.C f (Kernel.Prelude.Maybe Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator)),
    conditionType :: (B.C f Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType),
    conditionValue :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    dropSpecialLocationIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    id :: (B.C f Kernel.Prelude.Text),
    journeyId :: (B.C f Kernel.Prelude.Text),
    order :: (B.C f Kernel.Prelude.Int),
    pickupSpecialLocationIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    rewardConfigId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    rewardType :: (B.C f Domain.Types.IncentiveJourneyMilestone.MilestoneRewardType),
    rewardValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyMilestoneT where
  data PrimaryKey IncentiveJourneyMilestoneT f = IncentiveJourneyMilestoneId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyMilestoneId . id

type IncentiveJourneyMilestone = IncentiveJourneyMilestoneT Identity

$(enableKVPG (''IncentiveJourneyMilestoneT) [('id)] [[('journeyId)]])

$(mkTableInstances (''IncentiveJourneyMilestoneT) "incentive_journey_milestone")
