{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.UserCohortMapping where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data UserCohortMappingT f = UserCohortMappingT
  { cohortMappingId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    isTestGroup :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    userId :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table UserCohortMappingT where
  data PrimaryKey UserCohortMappingT f = UserCohortMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = UserCohortMappingId . id

type UserCohortMapping = UserCohortMappingT Identity

$(enableKVPG (''UserCohortMappingT) [('id)] [[('cohortMappingId)], [('userId)]])

$(mkTableInstancesGenericSchema (''UserCohortMappingT) "user_cohort_mapping")
