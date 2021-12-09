module Core.OnSearch.Category (Category (..)) where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Descriptor (Descriptor)

data Category = Category
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, FromJSON)
