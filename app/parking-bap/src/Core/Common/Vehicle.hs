module Core.Common.Vehicle where

import Beckn.Prelude

newtype Vehicle = Vehicle
  { registration :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
