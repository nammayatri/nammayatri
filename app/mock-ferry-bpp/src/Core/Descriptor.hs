module Core.Descriptor where

import Beckn.Prelude
import Data.OpenApi (ToSchema)

newtype DescriptorId = DescriptorId
  { name :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)

newtype DescriptorCode = DescriptorCode
  { code :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
