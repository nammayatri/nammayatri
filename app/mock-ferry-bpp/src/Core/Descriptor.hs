module Core.Descriptor where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Relude

newtype DescriptorId = DescriptorId
  { name :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)

newtype DescriptorCode = DescriptorCode
  { code :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
