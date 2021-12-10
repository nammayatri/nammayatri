module Beckn.Types.Core.Taxi.Search.Time where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example Time where
  example =
    Time
      { timestamp = example
      }
