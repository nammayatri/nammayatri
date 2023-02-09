{-# LANGUAGE StandaloneDeriving #-}

module Beckn.Spec.Confirm (module Beckn.Spec.Confirm, module Reexport) where

import Beckn.Spec.Common.Billing
import Beckn.Spec.Common.Payment
import Beckn.Spec.Common.ProviderId
import Beckn.Spec.Common.Quotation
import Beckn.Spec.Confirm.Item as Reexport
import Data.OpenApi hiding (items)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

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
    amount :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, PrettyShow, FromJSON, ToJSON)

deriving anyclass instance PrettyShow (Payment Params)

instance ToSchema Params where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

rupeeParams :: HighPrecMoney -> Params
rupeeParams = Params "INR"
