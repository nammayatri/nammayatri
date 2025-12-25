{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverGullakAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverGullakAssociationT f = DriverGullakAssociationT
  { driverId :: B.C f Kernel.Prelude.Text,
    gullakToken :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    tokenExpiry :: B.C f Kernel.Prelude.UTCTime,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGullakAssociationT where
  data PrimaryKey DriverGullakAssociationT f = DriverGullakAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverGullakAssociationId . driverId

type DriverGullakAssociation = DriverGullakAssociationT Identity

$(enableKVPG ''DriverGullakAssociationT ['driverId] [])

$(mkTableInstances ''DriverGullakAssociationT "driver_gullak_association")
