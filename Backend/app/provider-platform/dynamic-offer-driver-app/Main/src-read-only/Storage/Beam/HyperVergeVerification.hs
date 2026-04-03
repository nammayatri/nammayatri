{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.HyperVergeVerification where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.External.Encryption
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data HyperVergeVerificationT f
    = HyperVergeVerificationT {airConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                               docType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
                               documentImageId1 :: (B.C f Kernel.Prelude.Text),
                               documentImageId2 :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                               documentNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                               documentNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                               driverDateOfBirth :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                               driverId :: (B.C f Kernel.Prelude.Text),
                               hypervergeResponse :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               id :: (B.C f Kernel.Prelude.Text),
                               imageExtractionValidation :: (B.C f Domain.Types.IdfyVerification.ImageExtractionValidation),
                               issueDateOnDoc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                               nameOnCard :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               oxygen :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                               requestId :: (B.C f Kernel.Prelude.Text),
                               retryCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                               status :: (B.C f Kernel.Prelude.Text),
                               transactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
                               ventilator :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                               merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                               merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                               createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table HyperVergeVerificationT
    where data PrimaryKey HyperVergeVerificationT f = HyperVergeVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = HyperVergeVerificationId . id
type HyperVergeVerification = HyperVergeVerificationT Identity

$(enableKVPG (''HyperVergeVerificationT) [('id)] [[('driverId)], [('requestId)]])

$(mkTableInstances (''HyperVergeVerificationT) "hyperverge_verification")

