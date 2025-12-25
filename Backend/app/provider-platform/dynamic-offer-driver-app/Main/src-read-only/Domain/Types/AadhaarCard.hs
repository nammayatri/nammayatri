{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AadhaarCard where

import Data.Aeson
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AadhaarCard = AadhaarCard
  { aadhaarBackImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    aadhaarFrontImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    aadhaarNumberHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverGender :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maskedAadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON)
