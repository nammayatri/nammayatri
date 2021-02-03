module Types.API.Ride where

import EulerHS.Prelude

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Generic, ToJSON)