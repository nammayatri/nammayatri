module Core.OnConfirm.Params where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue
import Core.Payment

data Params = Params
  { transaction_id :: Text,
    transaction_status :: TrStatus,
    amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)


