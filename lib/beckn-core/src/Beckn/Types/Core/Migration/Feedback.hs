{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Feedback (Feedback) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Rating (Rating)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Data.Aeson.TH (deriveJSON)
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

deriveJSON stripAllLensPrefixOptions ''Feedback
deriveJSON constructorsToLowerOptions ''FeedbackType
