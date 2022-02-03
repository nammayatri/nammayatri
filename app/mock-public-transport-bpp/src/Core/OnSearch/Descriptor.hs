module Core.OnSearch.Descriptor where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Image (Image (..))

data Descriptor = Descriptor
  { name :: Text,
    code :: Text,
    symbol :: Text,
    short_desc :: Text,
    long_desc :: Text,
    images :: [Image]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

newtype DescriptorId = DescriptorId
  { name :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
