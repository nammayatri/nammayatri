module Core.SpecQuote where

import Beckn.Prelude
import Core.Breakup
import Core.Price

data SpecQuote = SpecQuote
  { price :: Price,
    breakup :: [Breakup]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
