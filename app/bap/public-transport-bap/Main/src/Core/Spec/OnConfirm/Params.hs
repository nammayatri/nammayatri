module Core.Spec.OnConfirm.Params where

import Core.Spec.Common.DecimalValue
import Core.Spec.Common.Payment
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema

data Params = Params
  { transaction_id :: Text,
    transaction_status :: TrStatus,
    amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, PrettyShow)

instance ToSchema Params where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
