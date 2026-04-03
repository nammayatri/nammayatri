{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BusinessLicense where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data BusinessLicenseT f
    = BusinessLicenseT {documentImageId :: (B.C f Kernel.Prelude.Text),
                        driverId :: (B.C f Kernel.Prelude.Text),
                        id :: (B.C f Kernel.Prelude.Text),
                        licenseExpiry :: (B.C f Kernel.Prelude.UTCTime),
                        licenseNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                        licenseNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                        verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BusinessLicenseT
    where data PrimaryKey BusinessLicenseT f = BusinessLicenseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BusinessLicenseId . id
type BusinessLicense = BusinessLicenseT Identity

$(enableKVPG (''BusinessLicenseT) [('id)] [[('documentImageId)]])

$(mkTableInstances (''BusinessLicenseT) "business_license")

