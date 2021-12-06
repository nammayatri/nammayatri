module Core.Confirm.Provider where

import Beckn.Prelude

newtype Provider = Provider
  { locations :: [Location]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype Location = Location
  { id :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)