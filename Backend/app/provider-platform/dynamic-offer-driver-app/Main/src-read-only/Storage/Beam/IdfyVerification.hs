{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IdfyVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IdfyVerificationT f = IdfyVerificationT
  { airConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    docType :: B.C f Domain.Types.DocumentVerificationConfig.DocumentType,
    documentImageId1 :: B.C f Kernel.Prelude.Text,
    documentImageId2 :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    documentNumberEncrypted :: B.C f Kernel.Prelude.Text,
    documentNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    driverDateOfBirth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    idfyResponse :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    imageExtractionValidation :: B.C f Domain.Types.IdfyVerification.ImageExtractionValidation,
    issueDateOnDoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    nameOnCard :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    oxygen :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    requestId :: B.C f Kernel.Prelude.Text,
    retryCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    status :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    ventilator :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IdfyVerificationT where
  data PrimaryKey IdfyVerificationT f = IdfyVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IdfyVerificationId . id

type IdfyVerification = IdfyVerificationT Identity

$(enableKVPG ''IdfyVerificationT ['id] [['driverId], ['requestId]])

$(mkTableInstances ''IdfyVerificationT "idfy_verification")
