module Beckn.Types.Core.Migration.Option where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Option = Option
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show)

instance FromJSON Option where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Option where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
