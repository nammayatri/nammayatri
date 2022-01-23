module Core.Provider where

import Data.Aeson
import Relude hiding (id)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
