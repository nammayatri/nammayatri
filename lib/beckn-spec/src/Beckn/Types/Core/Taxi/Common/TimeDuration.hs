module Beckn.Types.Core.Taxi.Common.TimeDuration where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))

newtype TimeDuration = TimeDuration
  { duration :: Text
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)
