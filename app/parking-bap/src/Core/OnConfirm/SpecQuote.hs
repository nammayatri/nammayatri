module Core.OnConfirm.SpecQuote where

import Beckn.Prelude
import Core.OnConfirm.Breakup
import Core.OnConfirm.Price

data SpecQuote = SpecQuote
  { price :: Price,
    breakup :: [Breakup]
  }
  deriving (Generic, FromJSON, ToJSON)