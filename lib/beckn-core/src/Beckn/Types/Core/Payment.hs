module Beckn.Types.Core.Payment where

import Beckn.Types.Core.Duration
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.PaymentEndpoint
import Beckn.Types.Core.State
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude hiding (State, state)

data Payment = Payment
  { transaction_id :: Maybe Text,
    _type :: Maybe Text, -- ON-ORDER, PRE-FULFILLMENT, ON-FULFILLMENT, POST-FULFILLMENT
    payer :: Maybe PaymentEndpoint,
    payee :: Maybe PaymentEndpoint,
    methods :: [Text], -- CASH, CHEQUE, DEMAND-DRAFT, UPI, RTGS, NEFT, IMPS
    amount :: MonetaryValue,
    state :: Maybe State,
    due_date :: Maybe UTCTime,
    duration :: Maybe Duration
  }
  deriving (Generic, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Payment where
  example =
    Payment
      { transaction_id = Just idExample,
        _type = Just "ON-ORDER",
        payer = example,
        payee = example,
        methods = ["CASH"],
        amount = example,
        state = Nothing,
        due_date = Just example,
        duration = Nothing
      }
