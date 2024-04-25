{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverBlockReason where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverBlockReasonT f = DriverBlockReasonT
  { blockReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    blockTimeInHours :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    reasonCode :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBlockReasonT where
  data PrimaryKey DriverBlockReasonT f = DriverBlockReasonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverBlockReasonId . reasonCode

type DriverBlockReason = DriverBlockReasonT Identity

$(enableKVPG (''DriverBlockReasonT) [('reasonCode)] [])

$(mkTableInstances (''DriverBlockReasonT) "driver_block_reason")
