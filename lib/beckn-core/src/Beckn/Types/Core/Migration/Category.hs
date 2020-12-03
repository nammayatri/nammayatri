module Beckn.Types.Core.Migration.Category (Category (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import EulerHS.Prelude

data Category = Category
  { _id :: Maybe Text,
    _parent_category_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _time :: Maybe Time,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

instance FromJSON Category where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Category where
  toJSON = genericToJSON stripLensPrefixOptions
