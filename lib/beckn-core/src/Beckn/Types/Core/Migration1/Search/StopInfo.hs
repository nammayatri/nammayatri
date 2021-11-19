module Beckn.Types.Core.Migration1.Search.StopInfo where

import Beckn.Types.Core.Migration1.Search.Gps (Gps)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

newtype StopInfo = StopInfo
  { gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example StopInfo where
  example =
    StopInfo
      { gps = example
      }
