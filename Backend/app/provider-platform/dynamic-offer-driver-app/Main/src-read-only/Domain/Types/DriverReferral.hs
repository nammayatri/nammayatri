{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverReferral where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverReferral = DriverReferral
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    dynamicReferralCode :: Kernel.Prelude.Maybe Data.Text.Text,
    dynamicReferralCodeValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    linkedAt :: Kernel.Prelude.UTCTime,
    referralCode :: Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral,
    role :: Domain.Types.Person.Role,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
