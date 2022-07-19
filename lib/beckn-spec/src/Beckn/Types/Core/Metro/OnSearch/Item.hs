module Beckn.Types.Core.Metro.OnSearch.Item where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Price (Price)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price,
    fulfillment_id :: Text,
    matched :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
