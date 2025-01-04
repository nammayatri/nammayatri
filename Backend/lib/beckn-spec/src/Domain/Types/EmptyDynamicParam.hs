module Domain.Types.EmptyDynamicParam where

import Data.Aeson
import Kernel.Prelude

data EmptyDynamicParam = EmptyDynamicParam
  deriving (Generic, ToSchema, FromJSON, Show, Eq, Ord, Read)

instance ToJSON EmptyDynamicParam where
  toJSON EmptyDynamicParam = object []
