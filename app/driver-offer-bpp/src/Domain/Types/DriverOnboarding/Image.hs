module Domain.Types.DriverOnboarding.Image where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.Person

data ImageType = DriverLicense | VehicleRegistrationCertificate
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data Image = Image
  { id :: Id Image,
    personId :: Id Person,
    organizationId :: Id Organization,
    s3Path :: Text,
    imageType :: ImageType,
    isValid :: Bool,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
