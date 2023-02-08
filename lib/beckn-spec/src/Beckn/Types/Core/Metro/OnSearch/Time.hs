module Beckn.Types.Core.Metro.OnSearch.Time where

import Kernel.Prelude

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
