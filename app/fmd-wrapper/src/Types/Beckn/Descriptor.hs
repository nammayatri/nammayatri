module Types.Beckn.Descriptor (Descriptor (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Descriptor = Descriptor
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show, Eq, ToSchema, ToJSON, FromJSON)
