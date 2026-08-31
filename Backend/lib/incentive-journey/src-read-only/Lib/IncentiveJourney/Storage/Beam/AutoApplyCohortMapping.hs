{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.AutoApplyCohortMapping where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data AutoApplyCohortMappingT f = AutoApplyCohortMappingT
  { allowIfNoMapping :: (B.C f Kernel.Prelude.Bool),
    cohortJourneyMappingId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table AutoApplyCohortMappingT where
  data PrimaryKey AutoApplyCohortMappingT f = AutoApplyCohortMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AutoApplyCohortMappingId . id

type AutoApplyCohortMapping = AutoApplyCohortMappingT Identity

$(enableKVPG (''AutoApplyCohortMappingT) [('id)] [[('cohortJourneyMappingId)]])

$(mkTableInstancesGenericSchema (''AutoApplyCohortMappingT) "auto_apply_cohort_mapping")
