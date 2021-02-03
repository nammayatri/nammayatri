module Types.API.Ride where

import Data.Swagger
import EulerHS.Prelude



data NotificationStatus
  = ACCEPT | REJECT
  deriving (Generic, ToJSON, ToSchema)