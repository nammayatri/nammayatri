module Beckn.Types.Core.Migration.Support where

import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import EulerHS.Prelude

data Support = Support
  { _type :: Maybe SupportType,
    _ref_id :: Maybe Text,
    _channels :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Support where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Support where
  toJSON = genericToJSON stripAllLensPrefixOptions

data SupportType
  = ORDER
  | BILLING
  | FULFILLMENT
  deriving (Generic, Show, Eq)

instance FromJSON SupportType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON SupportType where
  toJSON = genericToJSON constructorsToLowerOptions
