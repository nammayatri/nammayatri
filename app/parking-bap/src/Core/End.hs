module Core.End where

import Beckn.Prelude
import Core.Time

newtype End = End
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
