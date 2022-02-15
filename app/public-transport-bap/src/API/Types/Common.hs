module API.Types.Common where

import Beckn.Prelude

data Gps = Gps
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, FromJSON, ToSchema)
