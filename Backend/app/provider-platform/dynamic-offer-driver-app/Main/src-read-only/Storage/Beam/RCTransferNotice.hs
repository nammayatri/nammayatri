{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RCTransferNotice where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RCTransferNoticeT f = RCTransferNoticeT
  { acknowledged :: (B.C f Kernel.Prelude.Bool),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    hasOtherVehicles :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    newDriverId :: (B.C f Kernel.Prelude.Text),
    previousDriverId :: (B.C f Kernel.Prelude.Text),
    rcId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RCTransferNoticeT where
  data PrimaryKey RCTransferNoticeT f = RCTransferNoticeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RCTransferNoticeId . id

type RCTransferNotice = RCTransferNoticeT Identity

$(enableKVPG (''RCTransferNoticeT) [('id)] [[('previousDriverId)]])

$(mkTableInstances (''RCTransferNoticeT) "rc_transfer_notice")
