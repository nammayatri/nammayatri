module Beckn.Types.Core.Migration.State (State (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State)

data State = State
  { descriptor :: Maybe Descriptor,
    updated_at :: Maybe UTCTime,
    updated_by :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON State where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON State where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
