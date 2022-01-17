module Core.OnConfirm.Start where

import Beckn.Prelude
import Core.OnConfirm.Contact
import Core.OnConfirm.StartLocation
import Core.OnConfirm.Time

data Start = Start
  { location :: StartLocation,
    contact :: Contact,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
