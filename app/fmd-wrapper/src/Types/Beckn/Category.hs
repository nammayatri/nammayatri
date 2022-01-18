module Types.Beckn.Category (Category (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Types.Beckn.Descriptor (Descriptor)

data Category = Category
  { id :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
