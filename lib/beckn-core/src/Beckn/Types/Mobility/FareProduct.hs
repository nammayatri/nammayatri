module Beckn.Types.Mobility.FareProduct where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude hiding (id)

data FareProduct = FareProduct
  { id :: Text,
    descriptor :: Descriptor,
    policy_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example FareProduct where
  example =
    FareProduct
      { id = idExample,
        descriptor = example,
        policy_id = idExample
      }
