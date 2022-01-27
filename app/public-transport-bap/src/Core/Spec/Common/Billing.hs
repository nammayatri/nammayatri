module Core.Spec.Common.Billing where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)

newtype Billing = Billing
  { name :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema, PrettyShow)
