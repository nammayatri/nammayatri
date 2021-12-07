module Beckn.Types.Core.Cabs.Search.StartInfo where

import Beckn.Types.Core.Cabs.Search.Gps (Gps)
import Beckn.Types.Core.Cabs.Search.Time (Time)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data StartInfo = StartInfo
  { gps :: Gps,
    time :: Time
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example StartInfo where
  example =
    StartInfo
      { gps = example,
        time = example
      }
