{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.CohortDetails where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data CohortDetailsT f = CohortDetailsT {createdAt :: (B.C f Kernel.Prelude.UTCTime), id :: (B.C f Kernel.Prelude.Text), name :: (B.C f Kernel.Prelude.Text), updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
  deriving (Generic, B.Beamable)

instance B.Table CohortDetailsT where
  data PrimaryKey CohortDetailsT f = CohortDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CohortDetailsId . id

type CohortDetails = CohortDetailsT Identity

$(enableKVPG (''CohortDetailsT) [('id)] [])

$(mkTableInstancesGenericSchema (''CohortDetailsT) "cohort_details")
