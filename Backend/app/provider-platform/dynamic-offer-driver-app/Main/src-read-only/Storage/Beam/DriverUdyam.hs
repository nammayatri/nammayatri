{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverUdyam where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Kernel.Types.Documents
import qualified Domain.Types.DriverPanCard
import qualified Database.Beam as B



data DriverUdyamT f
    = DriverUdyamT {driverId :: (B.C f Kernel.Prelude.Text),
                    enterpriseName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    enterpriseType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    id :: (B.C f Kernel.Prelude.Text),
                    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                    udyamNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                    udyamNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                    verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
                    verifiedBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy)),
                    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                    createdAt :: (B.C f Kernel.Prelude.UTCTime),
                    updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverUdyamT
    where data PrimaryKey DriverUdyamT f = DriverUdyamId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverUdyamId . id
type DriverUdyam = DriverUdyamT Identity

$(enableKVPG (''DriverUdyamT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverUdyamT) "driver_udyam")

