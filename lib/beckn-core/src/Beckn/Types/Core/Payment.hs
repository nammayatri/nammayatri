module Beckn.Types.Core.Payment where

import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.PaymentEndpoint
import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.State
import Beckn.Utils.Common
import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude hiding (State)

data Payment = Payment
  { transaction_id :: Text, -- Transaction ID of the payment
    payer :: Maybe PaymentEndpoint,
    payee :: PaymentEndpoint,
    terms :: PaymentPolicy,
    amount :: Maybe MonetaryValue,
    state :: Maybe State,
    due_date :: Maybe LocalTime,
    scheduled_date :: Maybe LocalTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example Payment where
  example =
    Payment
      { transaction_id = idExample,
        payer = example,
        payee = example,
        terms = example,
        amount = Nothing,
        state = Nothing,
        due_date = Nothing,
        scheduled_date = Nothing
      }
