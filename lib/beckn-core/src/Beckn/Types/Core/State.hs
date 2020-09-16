module Beckn.Types.Core.State where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Common
import Data.Aeson (Value)
import Data.Time
import EulerHS.Prelude hiding (State)

data State = State
  { _descriptor :: Descriptor,
    _updated_at :: Maybe UTCTime,
    _updated_by :: Maybe Text,
    _update_metadata :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON State where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON State where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example State where
  example =
    State
      { _descriptor = example,
        _updated_at = example,
        _updated_by = Nothing,
        _update_metadata = Nothing
      }
