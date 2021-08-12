module Beckn.Types.Core.Billing where

import Beckn.Types.Core.Address
import Beckn.Types.Core.Duration
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Billing = Billing
  { customer :: Person,
    address :: Address,
    period :: Duration,
    tax_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Billing where
  example =
    Billing
      { customer = example,
        address = example,
        period = example,
        tax_number = Just "obq12345"
      }
