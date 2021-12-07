module Beckn.Types.Core.Cabs.Common.Payment
  ( Payment (..),
    Params (..),
  )
where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))

newtype Payment = Payment
  { params :: Params
  }
  deriving (Generic, Show, ToSchema)

newtype Params = Params
  { amount :: Double
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

instance Example Params where
  example =
    Params
      { amount = 123
      }

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Payment where
  example =
    Payment
      { params = example
      }
