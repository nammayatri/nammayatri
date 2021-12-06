module Core.Confirm.Time where

import Beckn.Prelude

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
