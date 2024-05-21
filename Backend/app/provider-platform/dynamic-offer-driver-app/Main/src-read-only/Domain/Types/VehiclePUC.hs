{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehiclePUC where

import Data.Aeson
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehiclePUC = VehiclePUC
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehiclePUC.VehiclePUC,
    pucExpiry :: Kernel.Prelude.UTCTime,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
