{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.Common

data CohortJourneyMappingT f = CohortJourneyMappingT
  { cohortId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    journeyId :: B.C f Kernel.Prelude.Text,
    startDate :: B.C f Kernel.Prelude.UTCTime,
    streakEndRewardExpirationAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    streakEndRewardType :: B.C f (Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.Common.MilestoneRewardType),
    streakEndRewardValue :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    streakRange :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CohortJourneyMappingT where
  data PrimaryKey CohortJourneyMappingT f = CohortJourneyMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CohortJourneyMappingId . id

type CohortJourneyMapping = CohortJourneyMappingT Identity

$(enableKVPG ''CohortJourneyMappingT ['id] [['cohortId], ['journeyId]])

$(mkTableInstancesGenericSchema ''CohortJourneyMappingT "cohort_journey_mapping")
