module Core.OnConfirm.Breakup where

import Beckn.Prelude
import Core.OnConfirm.Price

data Breakup = Breakup
  { title :: Text,
    price :: Price
  }
  deriving (Generic, FromJSON)