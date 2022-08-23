{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Rating where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import Domain.Types.Ride.Type (Ride)
import EulerHS.Prelude hiding (id)

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
