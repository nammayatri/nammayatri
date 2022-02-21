{-# LANGUAGE StandaloneDeriving #-}

module Core.Spec.Confirm (module Core.Spec.Confirm, module Reexport) where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.Billing
import Core.Spec.Common.DecimalValue
import Core.Spec.Common.Payment
import Core.Spec.Common.ProviderId
import Core.Spec.Common.Quotation
import Core.Spec.Confirm.Item as Reexport
import Data.Aeson
import Data.OpenApi hiding (items)

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow, ToSchema)

data Order = Order
  { provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Params = Params
  { currency :: Text,
    amount :: Amount
  }
  deriving (Generic, Eq, Show, PrettyShow)

deriving anyclass instance PrettyShow (Payment Params)

instance ToSchema Params where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

rupeeParams :: Amount -> Params
rupeeParams = Params "INR"

instance FromJSON Params where
  parseJSON = withObject "params" $ \obj -> do
    currency <- obj .: "currency"
    decimalValue <- obj .: "amount"
    amount <- maybe (fail "invalid params value") pure $ convertDecimalValueToAmount decimalValue
    pure Params {..}

instance ToJSON Params where
  toJSON p =
    object
      [ "currency" .= p.currency,
        "amount" .= convertAmountToDecimalValue (p.amount)
      ]
