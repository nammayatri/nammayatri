module Beckn.Types.Core.Migration1.Search
  ( module Beckn.Types.Core.Migration1.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.Search.Gps as Reexport
import Beckn.Types.Core.Migration1.Search.Intent as Reexport
import Beckn.Types.Core.Migration1.Search.StartInfo as Reexport
import Beckn.Types.Core.Migration1.Search.StopInfo as Reexport
import Beckn.Types.Core.Migration1.Search.Tags as Reexport
import Beckn.Types.Core.Migration1.Search.Time as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example SearchMessage where
  example =
    SearchMessage
      { intent = example
      }
