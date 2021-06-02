module Beckn.Types.Core.PaymentEndpoint where

import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data PaymentEndpoint = PaymentEndpoint
  { _type :: Text, -- "bank_account", "vpa", "person"
    bank_account :: Maybe BankAccount,
    vpa :: Maybe Text, -- Virtual Payment Address like a UPI address
    person :: Maybe Person
  }
  deriving (Generic, Show)

instance FromJSON PaymentEndpoint where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PaymentEndpoint where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example PaymentEndpoint where
  example =
    PaymentEndpoint
      { _type = "vpa",
        bank_account = Nothing,
        vpa = Just "someone@virtualAdress",
        person = Nothing
      }

data BankAccount = BankAccount
  { account_number :: Text,
    account_holder_name :: Text,
    ifsc_code :: Text
  }
  deriving (Generic, Show)

instance FromJSON BankAccount where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BankAccount where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example BankAccount where
  example =
    BankAccount
      { account_number = "1234567890",
        account_holder_name = "account holder",
        ifsc_code = "sbi123456"
      }
