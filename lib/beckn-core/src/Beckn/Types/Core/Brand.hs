module Beckn.Types.Core.Brand where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude hiding (id)

data Brand = Brand
  { id :: Text,
    parent_brand_id :: Maybe Text,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Brand where
  example =
    Brand
      { id = idExample,
        parent_brand_id = Just idExample,
        descriptor = example
      }
