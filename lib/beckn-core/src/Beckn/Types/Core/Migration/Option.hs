module Beckn.Types.Core.Migration.Option where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import EulerHS.Prelude

data Option = Option
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show)

instance FromJSON Option where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Option where
  toJSON = genericToJSON stripLensPrefixOptions
