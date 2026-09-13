{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone

data IncentiveJourneyMilestoneT f = IncentiveJourneyMilestoneT
  { areaType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    conditionOperator :: B.C f (Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionOperator),
    conditionType :: B.C f Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.MilestoneConditionType,
    conditionValue :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    journeyId :: B.C f Kernel.Prelude.Text,
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    order :: B.C f Kernel.Prelude.Int,
    rewardExpirationAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    rewardType :: B.C f Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType,
    rewardValue :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    serviceTierType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    timeBounds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyMilestoneT where
  data PrimaryKey IncentiveJourneyMilestoneT f = IncentiveJourneyMilestoneId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyMilestoneId . id

type IncentiveJourneyMilestone = IncentiveJourneyMilestoneT Identity

$(enableKVPG ''IncentiveJourneyMilestoneT ['id] [['journeyId]])

$(mkTableInstancesGenericSchema ''IncentiveJourneyMilestoneT "incentive_journey_milestone")
