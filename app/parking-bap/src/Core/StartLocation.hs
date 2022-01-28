module Core.StartLocation where

import Beckn.Prelude
import Core.Descriptor

data StartLocation = StartLocation
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
