module Beckn.Types.Core.Migration.Feedback (Feedback) where

import EulerHS.Prelude

data Feedback = Feedback
  { _id :: Maybe Text,
    _descriptor :: Maybe Text,
    _parent_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Feedback where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Feedback where
  toJSON = genericToJSON stripAllLensPrefixOptions
