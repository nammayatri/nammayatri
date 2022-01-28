module Core.Breakup where

import Beckn.Prelude
import Core.Price

data Breakup = Breakup
  { title :: Text,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
