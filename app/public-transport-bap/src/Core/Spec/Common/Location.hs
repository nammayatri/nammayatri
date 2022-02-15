module Core.Spec.Common.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps (Gps)

data Location = Location
  { id :: Text,
    gps :: Gps,
    stop_code :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
