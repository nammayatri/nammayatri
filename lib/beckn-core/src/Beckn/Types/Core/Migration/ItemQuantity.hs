module Beckn.Types.Core.Migration.ItemQuantity where

import Beckn.Types.Core.Migration.Scalar
import EulerHS.Prelude hiding (maximum, minimum)

data Quantity = Quantity
  { count :: Maybe Integer,
    measure :: Maybe Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data ItemQuantity = ItemQuantity
  { allocated :: Maybe Quantity,
    available :: Maybe Quantity,
    maximum :: Maybe Quantity,
    minimum :: Maybe Quantity,
    selected :: Maybe Quantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)

emptyItemQuantity :: ItemQuantity
emptyItemQuantity = ItemQuantity Nothing Nothing Nothing Nothing Nothing
