{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleNOC where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data VehicleNOCT f
    = VehicleNOCT {documentImageId :: (B.C f Kernel.Prelude.Text),
                   driverId :: (B.C f Kernel.Prelude.Text),
                   id :: (B.C f Kernel.Prelude.Text),
                   nocExpiry :: (B.C f Kernel.Prelude.UTCTime),
                   nocNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                   nocNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                   rcId :: (B.C f Kernel.Prelude.Text),
                   verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
                   merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VehicleNOCT
    where data PrimaryKey VehicleNOCT f = VehicleNOCId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = VehicleNOCId . id
type VehicleNOC = VehicleNOCT Identity

$(enableKVPG (''VehicleNOCT) [('id)] [[('documentImageId)], [('rcId)]])

$(mkTableInstances (''VehicleNOCT) "vehicle_noc")

