{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverLicense where

import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data DriverLicenseT f = DriverLicenseT
  { classOfVehicles :: B.C f [Kernel.Prelude.Text],
    consent :: B.C f Kernel.Prelude.Bool,
    consentTimestamp :: B.C f Kernel.Prelude.UTCTime,
    dateOfIssue :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    documentImageId1 :: B.C f Kernel.Prelude.Text,
    documentImageId2 :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverDob :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    driverName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    failedRules :: B.C f [Kernel.Prelude.Text],
    id :: B.C f Kernel.Prelude.Text,
    licenseExpiry :: B.C f Kernel.Prelude.UTCTime,
    licenseNumberEncrypted :: B.C f Kernel.Prelude.Text,
    licenseNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    verificationStatus :: B.C f Kernel.Types.Documents.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverLicenseT where
  data PrimaryKey DriverLicenseT f = DriverLicenseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverLicenseId . id

type DriverLicense = DriverLicenseT Identity

$(enableKVPG ''DriverLicenseT ['id] [['driverId], ['licenseNumberHash]])

$(mkTableInstances ''DriverLicenseT "driver_license")
