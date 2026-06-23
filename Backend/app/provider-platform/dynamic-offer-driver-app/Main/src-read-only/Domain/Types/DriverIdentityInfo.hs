{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverIdentityInfo where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Verification.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverIdentityInfo = DriverIdentityInfo
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    addressDocumentType :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.AddressDocumentType,
    addressState :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.IndianState,
    courtRecord :: Kernel.Prelude.Maybe Domain.Types.DriverIdentityInfo.CourtRecordResult,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nomineeDob :: Kernel.Prelude.Maybe Data.Time.Day,
    nomineeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nomineeRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CourtRecordResult = CourtRecordResult {errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text, result :: Kernel.Prelude.Maybe Kernel.External.Verification.Types.CRCVerificationResponse}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
