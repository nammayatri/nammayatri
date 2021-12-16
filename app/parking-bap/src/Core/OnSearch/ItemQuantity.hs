module Core.OnSearch.ItemQuantity where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Scalar

data Quantity = Quantity
  { count :: Int,
    measure :: Maybe Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype ItemQuantity = ItemQuantity
  { available :: Quantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)
