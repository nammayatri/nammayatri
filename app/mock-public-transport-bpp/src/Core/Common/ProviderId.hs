module Core.Common.ProviderId where

import Data.Aeson
import Relude hiding (id)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
