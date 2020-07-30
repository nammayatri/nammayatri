module Beckn.Types.Core.PaymentEndpoint where

import Beckn.Types.Core.Person
import Beckn.Utils.Common
import EulerHS.Prelude

data PaymentEndpoint = PaymentEndpoint
  { _type :: Text, -- "bank_account", "vpa", "person"
    _bank_account :: Maybe BankAccount,
    _vpa :: Maybe Text, -- Virtual Payment Address like a UPI address
    _person :: Maybe Person
  }
  deriving (Generic, Show)

instance FromJSON PaymentEndpoint where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PaymentEndpoint where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example PaymentEndpoint where
  example =
    PaymentEndpoint
      { _type = "vpa",
        _bank_account = Nothing,
        _vpa = Just "someone@virtualAdress",
        _person = Nothing
      }

data BankAccount = BankAccount
  { _account_number :: Text,
    _account_holder_name :: Text,
    _ifsc_code :: Text
  }
  deriving (Generic, Show)

instance FromJSON BankAccount where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON BankAccount where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example BankAccount where
  example =
    BankAccount
      { _account_number = "1234567890",
        _account_holder_name = "account holder",
        _ifsc_code = "sbi123456"
      }
