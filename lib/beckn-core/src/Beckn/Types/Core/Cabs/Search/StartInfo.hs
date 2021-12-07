module Beckn.Types.Core.Cabs.Search.StartInfo where

import Beckn.Types.Core.Cabs.Search.Location (Location)
import Beckn.Types.Core.Cabs.Search.Time (Time)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data StartInfo = StartInfo
  { location :: Location,
    time :: Time
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example StartInfo where
  example =
    StartInfo
      { location = example,
        time = example
      }
