module Beckn.Types.Core.Migration.Cancellation (Cancellation (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Option (Option)
import Beckn.Types.Core.Migration.Policy (Policy)
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude

data Cancellation = Cancellation
  { _type :: Maybe CancellationType,
    ref_id :: Maybe Text,
    policies :: Maybe [Policy],
    time :: Maybe UTCTime,
    cancelled_by :: Maybe Text,
    reasons :: Maybe Option,
    selected_reason :: Maybe IdObject,
    additional_description :: Maybe Descriptor
  }
  deriving (Generic, Show)

data CancellationType = FULL | PARTIAL
  deriving (Generic, Show)

instance FromJSON Cancellation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Cancellation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON CancellationType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON CancellationType where
  toJSON = genericToJSON constructorsToLowerOptions
