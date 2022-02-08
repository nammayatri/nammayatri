module Core.Common.Descriptor where

import Beckn.Prelude

data Descriptor = Descriptor
  { name :: Text,
    short_desc :: Text,
    images :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
