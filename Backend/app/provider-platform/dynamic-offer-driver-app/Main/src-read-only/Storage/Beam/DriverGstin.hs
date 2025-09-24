{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverGstin where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverPanCard
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data DriverGstinT f = DriverGstinT
  { address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    constitutionOfBusiness :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    dateOfLiability :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    documentImageId1 :: (B.C f Kernel.Prelude.Text),
    documentImageId2 :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    driverId :: (B.C f Kernel.Prelude.Text),
    driverName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    gstinEncrypted :: (B.C f Kernel.Prelude.Text),
    gstinHash :: (B.C f Kernel.External.Encryption.DbHash),
    id :: (B.C f Kernel.Prelude.Text),
    isProvisional :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    legalName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    panNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    tradeName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    typeOfRegistration :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    validFrom :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    validUpto :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
    verifiedBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGstinT where
  data PrimaryKey DriverGstinT f = DriverGstinId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverGstinId . id

type DriverGstin = DriverGstinT Identity

$(enableKVPG (''DriverGstinT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverGstinT) "driver_gstin")
