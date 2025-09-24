{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BusinessLicense where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data BusinessLicenseT f = BusinessLicenseT
  { documentImageId :: (B.C f Kernel.Prelude.Text),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    licenseExpiry :: (B.C f Kernel.Prelude.UTCTime),
    licenseNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    licenseNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BusinessLicenseT where
  data PrimaryKey BusinessLicenseT f = BusinessLicenseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BusinessLicenseId . id

type BusinessLicense = BusinessLicenseT Identity

$(enableKVPG (''BusinessLicenseT) [('id)] [[('documentImageId)]])

$(mkTableInstances (''BusinessLicenseT) "business_license")
