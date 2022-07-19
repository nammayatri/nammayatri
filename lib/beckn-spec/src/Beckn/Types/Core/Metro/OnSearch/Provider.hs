module Beckn.Types.Core.Metro.OnSearch.Provider where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Fulfillment
import Beckn.Types.Core.Metro.OnSearch.Item
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    items :: [Item],
    fulfillments :: [Fulfillment]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
