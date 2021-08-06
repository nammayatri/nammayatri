module Beckn.Types.Core.Migration.Option where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import EulerHS.Prelude hiding (id)

data Option = Option
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)
