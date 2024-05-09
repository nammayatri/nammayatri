{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleFitnessCertificate where

import qualified Database.Beam as B
import qualified Domain.Types.IdfyVerification
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleFitnessCertificateT f = VehicleFitnessCertificateT
  { applicationNumberEncrypted :: B.C f Kernel.Prelude.Text,
    applicationNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    categoryOfVehicle :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    documentImageId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    fitnessExpiry :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    inspectingAuthority :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    inspectingOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    nextInspectionDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rcId :: B.C f Kernel.Prelude.Text,
    receiptDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    verificationStatus :: B.C f Domain.Types.IdfyVerification.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleFitnessCertificateT where
  data PrimaryKey VehicleFitnessCertificateT f = VehicleFitnessCertificateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleFitnessCertificateId . id

type VehicleFitnessCertificate = VehicleFitnessCertificateT Identity

$(enableKVPG ''VehicleFitnessCertificateT ['id] [])

$(mkTableInstances ''VehicleFitnessCertificateT "vehicle_fitness_certificate")
