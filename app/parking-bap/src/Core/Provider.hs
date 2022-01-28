module Core.Provider where

import Beckn.Prelude
import Core.Descriptor
import Core.Location

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
