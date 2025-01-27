{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderDriverCorrelation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RiderDriverCorrelationT f = RiderDriverCorrelationT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    favourite :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    mobileNumberEncrypted :: B.C f Kernel.Prelude.Text,
    mobileNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    riderDetailId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderDriverCorrelationT where
  data PrimaryKey RiderDriverCorrelationT f = RiderDriverCorrelationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RiderDriverCorrelationId <$> driverId <*> riderDetailId

type RiderDriverCorrelation = RiderDriverCorrelationT Identity

$(enableKVPG ''RiderDriverCorrelationT ['driverId, 'riderDetailId] [])

$(mkTableInstances ''RiderDriverCorrelationT "rider_driver_correlation")
