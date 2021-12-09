module Core.OnSearch.Descriptor where

import Beckn.Prelude

data Descriptor = Descriptor
  { name :: Text,
    short_desc :: Text,
    images :: [BaseUrl]
  }
  deriving (Generic, FromJSON)
