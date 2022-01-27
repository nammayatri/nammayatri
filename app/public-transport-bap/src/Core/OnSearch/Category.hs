module Core.OnSearch.Category where

import Beckn.Prelude
import Core.Descriptor (Descriptor)

data Category = Category
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
