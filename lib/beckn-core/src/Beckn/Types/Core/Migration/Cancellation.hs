module Beckn.Types.Core.Migration.Cancellation (Cancellation (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Option (Option)
import Beckn.Types.Core.Migration.Policy (Policy)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Cancellation = Cancellation
  { _type :: Maybe CancellationType,
    _ref_id :: Maybe Text,
    _policies :: [Policy],
    _time :: Maybe UTCTime,
    _cancelled_by :: Maybe Text,
    _reasons :: Maybe Option,
    _selected_reason :: Maybe IdObject,
    _additional_description :: Maybe Descriptor
  }
  deriving (Generic, Show)

data CancellationType = Full | Partial
  deriving (Generic, Show)

instance FromJSON Cancellation where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Cancellation where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON CancellationType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON CancellationType where
  toJSON = genericToJSON constructorsToLowerOptions
