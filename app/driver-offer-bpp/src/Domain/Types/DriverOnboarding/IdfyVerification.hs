module Domain.Types.DriverOnboarding.IdfyVerification where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data IdfyVerification = IdfyVerification
  { id :: Id IdfyVerification,
    documentImageId1 :: Id Image,
    documentImageId2 :: Maybe (Id Image),
    driverId :: Id Person,
    requestId :: Text,
    docType :: ImageType,
    status :: Text,
    idfyResponse :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
