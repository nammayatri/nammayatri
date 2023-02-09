module Domain.Types.Rating where

import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import Domain.Types.Ride (Ride)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data Rating = Rating
  { id :: Id Rating,
    rideId :: Id Ride,
    driverId :: Id Person,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
