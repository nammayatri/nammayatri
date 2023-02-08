module Beckn.Types.Core.Taxi.OnInit.OrderState where

import Kernel.Prelude

data OrderState = NEW | ACTIVE | CONFIRMED
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
