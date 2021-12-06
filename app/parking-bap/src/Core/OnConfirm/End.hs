module Core.OnConfirm.End where

import Beckn.Prelude
import Core.OnConfirm.Time

newtype End = End
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)