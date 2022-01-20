module Core.Provider where

import Beckn.Prelude

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
