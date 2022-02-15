module Core.Spec.OnSearch.Fares where

import Beckn.Prelude
import Core.Spec.Common.Price

data Fares = Fares
  { id :: Text,
    route_id :: Text,
    price :: Price
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
