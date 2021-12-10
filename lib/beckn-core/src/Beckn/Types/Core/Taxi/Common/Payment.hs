module Beckn.Types.Core.Taxi.Common.Payment
  ( Payment (..),
    Params (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))

newtype Payment = Payment
  { params :: Params
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

newtype Params = Params
  { amount :: DecimalValue
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

instance Example Params where
  example =
    Params
      { amount = 123
      }

instance Example Payment where
  example =
    Payment
      { params = example
      }
