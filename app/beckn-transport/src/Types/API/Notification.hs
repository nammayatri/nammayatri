module Types.API.Notification where

import Data.Time
import EulerHS.Prelude
import Types.API.Location

data NotificationDetailResponse = NotificationDetailResponse
  { productInstanceId :: Text,
    pickupLoc :: LocationInfo,
    dropLoc :: LocationInfo,
    etaForPickupLoc :: NominalDiffTime,
    distanceToPickupLoc :: Text,
    notificationExpiry :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)
