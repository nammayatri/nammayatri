{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IdfyVerification where

import qualified Database.Beam as B
import qualified Domain.Types.DriverOnboarding.Image
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IdfyVerificationT f = IdfyVerificationT
  { dashboardPassedVehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant),
    docType :: B.C f Domain.Types.DriverOnboarding.Image.ImageType,
    documentImageId1 :: B.C f Kernel.Prelude.Text,
    documentImageId2 :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    documentNumberEncrypted :: B.C f Text,
    documentNumberHash :: B.C f DbHash,
    driverDateOfBirth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    idfyResponse :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    imageExtractionValidation :: B.C f Domain.Types.IdfyVerification.ImageExtractionValidation,
    issueDateOnDoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    multipleRC :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    nameOnCard :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestId :: B.C f Kernel.Prelude.Text,
    retryCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    status :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IdfyVerificationT where
  data PrimaryKey IdfyVerificationT f = IdfyVerificationId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = IdfyVerificationId . id

type IdfyVerification = IdfyVerificationT Identity

$(enableKVPG ''IdfyVerificationT ['id] [['driverId], ['requestId]])

$(mkTableInstancesWithTModifier ''IdfyVerificationT "idfy_verification" [("multipleRC", "multiple_r_c")])
