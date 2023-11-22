module Beckn.Types.Core.Taxi.Common.Billing where

import Data.OpenApi hiding (email, name)
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Billing = Billing
  { name :: Maybe Text,
    email :: Maybe Text,
    phone :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, PrettyShow)

instance ToSchema Billing where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
