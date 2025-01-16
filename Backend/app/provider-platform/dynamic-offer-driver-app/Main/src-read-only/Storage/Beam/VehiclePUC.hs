{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehiclePUC where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data VehiclePUCT f = VehiclePUCT
  { documentImageId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    pucExpiry :: B.C f Kernel.Prelude.UTCTime,
    pucNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    pucNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    rcId :: B.C f Kernel.Prelude.Text,
    testDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    verificationStatus :: B.C f Kernel.Types.Documents.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehiclePUCT where
  data PrimaryKey VehiclePUCT f = VehiclePUCId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehiclePUCId . id

type VehiclePUC = VehiclePUCT Identity

$(enableKVPG ''VehiclePUCT ['id] [['rcId]])

$(mkTableInstances ''VehiclePUCT "vehicle_puc")
