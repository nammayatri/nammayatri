{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EstimateRevised where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EstimateRevisedT f = EstimateRevisedT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    parentSearchId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateRevisedT where
  data PrimaryKey EstimateRevisedT f = EstimateRevisedId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EstimateRevisedId . id

type EstimateRevised = EstimateRevisedT Identity

$(enableKVPG (''EstimateRevisedT) [('id)] [])

$(mkTableInstancesWithTModifier (''EstimateRevisedT) "estimate_revised" [])
