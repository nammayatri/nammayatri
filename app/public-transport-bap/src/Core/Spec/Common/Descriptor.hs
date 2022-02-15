module Core.Spec.Common.Descriptor where

import Beckn.Prelude

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
