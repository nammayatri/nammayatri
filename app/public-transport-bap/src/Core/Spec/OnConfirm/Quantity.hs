module Core.Spec.OnConfirm.Quantity where

import Beckn.Prelude
import Beckn.Utils.GenericPretty

newtype Quantity = Quantity
  { count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)
