module Beckn.Types.Core.Migration.Feedback (Feedback) where

import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Feedback = Feedback
  { id :: Maybe Text,
    descriptor :: Maybe Text,
    parent_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Feedback where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Feedback where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
