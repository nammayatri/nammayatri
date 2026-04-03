{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverPanCard where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DriverPanCard
import qualified Kernel.External.Encryption
import qualified Kernel.Types.Documents
import qualified Database.Beam as B



data DriverPanCardT f
    = DriverPanCardT {consent :: (B.C f Kernel.Prelude.Bool),
                      consentTimestamp :: (B.C f Kernel.Prelude.UTCTime),
                      docType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.PanType)),
                      documentImageId1 :: (B.C f Kernel.Prelude.Text),
                      documentImageId2 :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      driverDob :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      driverId :: (B.C f Kernel.Prelude.Text),
                      driverName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      driverNameOnGovtDB :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      failedRules :: (B.C f [Kernel.Prelude.Text]),
                      id :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      panAadhaarLinkage :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.PanAadhaarLinkage)),
                      panCardNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                      panCardNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                      verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
                      verifiedBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy)),
                      merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverPanCardT
    where data PrimaryKey DriverPanCardT f = DriverPanCardId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverPanCardId . id
type DriverPanCard = DriverPanCardT Identity

$(enableKVPG (''DriverPanCardT) [('id)] [[('driverId)], [('panCardNumberHash)]])

$(mkTableInstances (''DriverPanCardT) "driver_pan_card")

