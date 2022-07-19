module Beckn.Types.Core.Taxi.OnInit.OrderState where

import Beckn.Prelude

data OrderState = NEW | ACTIVE | CONFIRMED
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
