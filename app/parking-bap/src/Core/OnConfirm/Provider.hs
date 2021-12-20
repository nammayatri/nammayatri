module Core.OnConfirm.Provider where

import Beckn.Prelude
import Core.Location
import Core.OnConfirm.Descriptor

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location]
  }
  deriving (Generic, FromJSON, ToJSON)
