{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RCTransferNotice where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RCTransferNotice = RCTransferNotice
  { acknowledged :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    hasOtherVehicles :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.RCTransferNotice.RCTransferNotice,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    newDriverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    previousDriverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
