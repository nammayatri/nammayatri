module Core.Descriptor where

import Beckn.Prelude

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)