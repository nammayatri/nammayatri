{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.UserCohortMapping where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping

data UserCohortMappingT f = UserCohortMappingT
  { cohortMappingId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    enabled :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    optInPeriod :: B.C f (Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.UserCohortMapping.OptInPeriod),
    optedInAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    optedInBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    termsAcceptedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    userId :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table UserCohortMappingT where
  data PrimaryKey UserCohortMappingT f = UserCohortMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = UserCohortMappingId . id

type UserCohortMapping = UserCohortMappingT Identity

$(enableKVPG ''UserCohortMappingT ['id] [['cohortMappingId], ['userId]])

$(mkTableInstancesGenericSchema ''UserCohortMappingT "user_cohort_mapping")
