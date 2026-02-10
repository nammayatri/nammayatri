{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.StclMembership where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.StclMembership
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

data Address = Address
  { city :: Kernel.Prelude.Text,
    postalCode :: Kernel.Prelude.Text,
    stateName :: Kernel.Prelude.Text,
    streetAddress1 :: Kernel.Prelude.Text,
    streetAddress2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BankDetails = BankDetails {accountNumber :: Kernel.Prelude.Text, bankName :: Kernel.Prelude.Text, branch :: Kernel.Prelude.Text, ifscCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Declaration = Declaration {date :: Data.Time.Day, place :: Kernel.Prelude.Text, signature :: Kernel.Prelude.Text, termsAccepted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FuelType
  = EV
  | Petrol
  | Diesel
  | CNG
  | Other
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MembershipApplicationReq = MembershipApplicationReq
  { aadharNumber :: Kernel.Prelude.Text,
    address :: Address,
    amount :: Kernel.Types.Common.Money,
    bankDetails :: BankDetails,
    dateOfBirth :: Data.Time.Day,
    declaration :: Declaration,
    driverId :: Kernel.Prelude.Text,
    emailId :: Kernel.Prelude.Text,
    fatherMotherName :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    memberCategory :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    nomineeInfo :: NomineeInfo,
    numberOfShares :: Kernel.Prelude.Int,
    panNumber :: Kernel.Prelude.Text,
    paymentServiceType :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PaymentOrder.PaymentServiceType,
    vehicleInfo :: VehicleInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MembershipApplicationResp = MembershipApplicationResp {applicationId :: Kernel.Prelude.Text, message :: Kernel.Prelude.Text, status :: Domain.Types.StclMembership.ApplicationStatus, submittedAt :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MembershipDetailsResp = MembershipDetailsResp
  { aadharNumber :: Kernel.Prelude.Text,
    address :: Address,
    applicationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    applicationId :: Kernel.Prelude.Text,
    bankDetails :: BankDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Data.Time.Day,
    declaration :: Declaration,
    driverId :: Kernel.Prelude.Text,
    emailId :: Kernel.Prelude.Text,
    fatherMotherName :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    memberCategory :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    nomineeInfo :: NomineeInfo,
    numberOfShares :: Kernel.Prelude.Int,
    panNumber :: Kernel.Prelude.Text,
    shareEndCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    shareStartCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.StclMembership.ApplicationStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleInfo :: VehicleInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NomineeInfo = NomineeInfo {nomineeAadhar :: Kernel.Prelude.Text, nomineeName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo {fuelTypes :: [FuelType], vehicleType :: VehicleType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleType
  = TwoWheeler
  | ThreeWheeler
  | FourWheeler
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
