{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AadhaarCard where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data AadhaarCardT f = AadhaarCardT
  { aadhaarBackImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarFrontImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    consent :: B.C f Kernel.Prelude.Bool,
    consentTimestamp :: B.C f Kernel.Prelude.UTCTime,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    dateOfBirth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverGender :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverId :: B.C f Kernel.Prelude.Text,
    driverImage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverImagePath :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    maskedAadhaarNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    nameOnCard :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    verificationStatus :: B.C f Kernel.Types.Documents.VerificationStatus
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarCardT where
  data PrimaryKey AadhaarCardT f = AadhaarCardId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AadhaarCardId . driverId

type AadhaarCard = AadhaarCardT Identity

$(enableKVPG ''AadhaarCardT ['driverId] [])

$(mkTableInstances ''AadhaarCardT "aadhaar_card")
