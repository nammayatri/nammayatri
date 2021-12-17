module Core.OnSearch.Descriptor where

import Beckn.Prelude

data Descriptor = Descriptor
  { name :: Text,
    images :: [BaseUrl]
  }
  deriving (Generic, FromJSON, ToJSON, Show)
