module Core.Spec.Common.Time where

import Beckn.Prelude

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)