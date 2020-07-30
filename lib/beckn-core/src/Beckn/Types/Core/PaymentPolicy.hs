module Beckn.Types.Core.PaymentPolicy where

import Beckn.Types.Core.Duration
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data PaymentPolicy = PaymentPolicy
  { supported_currencies :: Maybe [Text],
    credit_type :: Text, -- "PREPAID", "POSTPAID", "POINT-OF-SALE"
    settlement_type :: Text, -- "PER-TRANSACTION", "BULK"
    credit_duration :: Duration,
    mode :: Text, -- "CASH", "CHEQUE", "DEMAD-DRAFT", "UPI", "RTGS", "NEFT", "IMPS"
    method :: Text, -- "ELECTRONIC", "NON-ELECTRONIC"
    penalty_terms :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example PaymentPolicy where
  example =
    PaymentPolicy
      { supported_currencies = Nothing,
        credit_type = "POSTPAID",
        settlement_type = "BULK",
        credit_duration = example,
        mode = "RTGS",
        method = "ELECTRONIC",
        penalty_terms = ["Delay in payment after due date will incur 10 INR per day of non-payment"]
      }
