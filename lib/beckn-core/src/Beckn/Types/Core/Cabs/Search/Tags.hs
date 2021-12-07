module Beckn.Types.Core.Cabs.Search.Tags
  ( Tags (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Common.DecimalValue as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Tags = Tags
  { distance :: DecimalValue
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Tags where
  example =
    Tags
      { distance = 12
      }
