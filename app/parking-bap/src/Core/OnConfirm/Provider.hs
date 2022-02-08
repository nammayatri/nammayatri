module Core.OnConfirm.Provider where

import Beckn.Prelude
import Core.Common.Descriptor
import Core.OnConfirm.Location

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
