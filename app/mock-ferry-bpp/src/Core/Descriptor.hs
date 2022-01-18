module Core.Descriptor where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Image (Image (..))
import Data.OpenApi (ToSchema)

data DescriptorDetails = DescriptorDetails
  { name :: Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Text,
    long_desc :: Maybe Text,
    images :: [Image]
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)

newtype DescriptorId = DescriptorId
  { name :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)

newtype DescriptorCode = DescriptorCode
  { code :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
