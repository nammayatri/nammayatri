module Core.Common.Time where

import Beckn.Prelude

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
