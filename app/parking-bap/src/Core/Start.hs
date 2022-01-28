module Core.Start where

import Beckn.Prelude
import Core.Contact
import Core.StartLocation
import Core.Time

data Start = Start
  { location :: StartLocation,
    contact :: Contact,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
