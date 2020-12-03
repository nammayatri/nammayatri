module Beckn.Types.Core.Migration.Policy where

import Beckn.Types.Core.Migration.Descriptor
import Beckn.Types.Core.Migration.Time
import EulerHS.Prelude

data Policy = Policy
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _parent_policy_id :: Maybe Text,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON Policy where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Policy where
  toJSON = genericToJSON stripAllLensPrefixOptions
