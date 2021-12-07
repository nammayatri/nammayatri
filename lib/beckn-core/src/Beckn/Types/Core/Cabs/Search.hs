module Beckn.Types.Core.Cabs.Search
  ( module Beckn.Types.Core.Cabs.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Search.Gps as Reexport
import Beckn.Types.Core.Cabs.Search.Intent as Reexport
import Beckn.Types.Core.Cabs.Search.StartInfo as Reexport
import Beckn.Types.Core.Cabs.Search.StopInfo as Reexport
import Beckn.Types.Core.Cabs.Search.Tags as Reexport
import Beckn.Types.Core.Cabs.Search.Time as Reexport
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
