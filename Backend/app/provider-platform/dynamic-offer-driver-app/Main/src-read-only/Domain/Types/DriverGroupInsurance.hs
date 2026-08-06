{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverGroupInsurance where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DriverGroupInsurance = DriverGroupInsurance
  { age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    dob :: Kernel.Prelude.Maybe Data.Time.Day,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    enabledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fullName :: Kernel.Prelude.Text,
    gender :: Kernel.Prelude.Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsuranceGender,
    id :: Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance,
    insuranceType :: Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType,
    lastExportedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    mobile :: Kernel.Prelude.Text,
    nomineeDob :: Kernel.Prelude.Maybe Data.Time.Day,
    nomineeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nomineeRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    secondBotCheckAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.DriverGroupInsurance.DriverGroupInsuranceStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverGroupInsuranceGender = Male | Female | Other deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DriverGroupInsuranceStatus = Draft | Verified | Enabled | Exported deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DriverGroupInsuranceType = GMC | GPA deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverGroupInsuranceGender))

$(mkHttpInstancesForEnum (''DriverGroupInsuranceGender))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverGroupInsuranceStatus))

$(mkHttpInstancesForEnum (''DriverGroupInsuranceStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverGroupInsuranceType))

$(mkHttpInstancesForEnum (''DriverGroupInsuranceType))
