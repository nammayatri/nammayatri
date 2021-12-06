module Core.Confirm.StartEnd where

import Beckn.Prelude
import Core.Confirm.Time (Time)

newtype StartEnd = StartEnd
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
