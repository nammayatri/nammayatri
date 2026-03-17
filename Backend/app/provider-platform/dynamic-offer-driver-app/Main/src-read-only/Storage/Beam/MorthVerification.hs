{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MorthVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MorthVerificationT f = MorthVerificationT
  { docType :: B.C f Domain.Types.DocumentVerificationConfig.DocumentType,
    documentNumberEncrypted :: B.C f Kernel.Prelude.Text,
    documentNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    driverDateOfBirth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    issueDateOnDoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    morthResponse :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Kernel.Prelude.Text,
    statusCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MorthVerificationT where
  data PrimaryKey MorthVerificationT f = MorthVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MorthVerificationId . id

type MorthVerification = MorthVerificationT Identity

$(enableKVPG ''MorthVerificationT ['id] [['docType], ['documentNumberHash], ['driverId], ['requestId]])

$(mkTableInstances ''MorthVerificationT "morth_verification")
