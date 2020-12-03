module Beckn.Types.Core.Migration.Feedback (Feedback) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Rating (Rating)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import EulerHS.Prelude

data Feedback = Feedback
  { _type :: Maybe FeedbackType,
    _ref_id :: Maybe Text,
    _rating :: Maybe Rating,
    _descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show)

data FeedbackType
  = ORDER
  | FULFILLMENT
  | ITEM
  | SUPPORT
  | PERSON
  deriving (Generic, Show)

instance FromJSON Feedback where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Feedback where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON FeedbackType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON FeedbackType where
  toJSON = genericToJSON constructorsToLowerOptions
