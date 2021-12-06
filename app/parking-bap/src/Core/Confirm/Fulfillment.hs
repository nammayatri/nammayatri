module Core.Confirm.Fulfillment where

import Beckn.Prelude
import Core.Confirm.StartEnd
import Core.Confirm.Vehicle

data Fulfillment = Fulfillment
  { start :: StartEnd,
    end :: StartEnd,
    vehicle :: Vehicle
  }
  deriving (Generic, ToJSON)
