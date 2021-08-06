module Beckn.Types.Core.Migration.AddOn (AddOn (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import EulerHS.Prelude hiding (id)

data AddOn = AddOn
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)
