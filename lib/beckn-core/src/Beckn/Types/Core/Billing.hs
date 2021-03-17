module Beckn.Types.Core.Billing where

import Beckn.Types.Core.Address
import Beckn.Types.Core.Duration
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import EulerHS.Prelude

data Billing = Billing
  { _customer :: Person,
    _address :: Address,
    _period :: Duration,
    _tax_number :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Billing where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Billing where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Billing where
  example =
    Billing
      { _customer = example,
        _address = example,
        _period = example,
        _tax_number = Just "obq12345"
      }
