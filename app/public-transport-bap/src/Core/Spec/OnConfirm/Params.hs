module Core.Spec.OnConfirm.Params where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Utils.GenericPretty (PrettyShow)
import Core.Spec.Common.Payment

data Params = Params
  { transaction_id :: Text,
    transaction_status :: TrStatus,
    amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema, PrettyShow)
