module Beckn.Types.Core.Migration.Policy where

import Beckn.Types.Core.Migration.Descriptor
import Beckn.Types.Core.Migration.Time
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Policy = Policy
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    parent_policy_id :: Maybe Text,
    time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON Policy where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Policy where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
