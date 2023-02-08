module Domain.Types.DriverOnboarding.Image where

import Domain.Types.DriverOnboarding.Error
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id

data ImageType = DriverLicense | VehicleRegistrationCertificate
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data Image = Image
  { id :: Id Image,
    personId :: Id Person,
    merchantId :: Id Merchant,
    s3Path :: Text,
    imageType :: ImageType,
    isValid :: Bool,
    failureReason :: Maybe DriverOnboardingError,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
