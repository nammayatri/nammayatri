module Beckn.Types.Core.Migration.Support where

import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON (constructorsToLowerOptions, stripPrefixUnderscoreIfAny)
import EulerHS.Prelude

data Support = Support
  { _type :: Maybe SupportType,
    ref_id :: Maybe Text,
    channels :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Support where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Support where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data SupportType
  = ORDER
  | BILLING
  | FULFILLMENT
  deriving (Generic, Show, Eq)

instance FromJSON SupportType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON SupportType where
  toJSON = genericToJSON constructorsToLowerOptions
