module Core.OnConfirm.Provider where

import Beckn.Prelude
import Core.OnConfirm.Descriptor
import Core.OnConfirm.ProviderLocation

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [ProviderLocation]
  }
  deriving (Generic, FromJSON)
