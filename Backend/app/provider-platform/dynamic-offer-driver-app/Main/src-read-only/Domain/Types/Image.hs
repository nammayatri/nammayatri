{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Image where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Error

data Image = Image
  { failureReason :: Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError,
    id :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    imageType :: Domain.Types.Image.ImageType,
    isValid :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    s3Path :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ImageType = DriverLicense | VehicleRegistrationCertificate deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ImageType))
