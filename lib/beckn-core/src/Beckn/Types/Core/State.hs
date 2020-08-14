module Beckn.Types.Core.State where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Common
import Data.Aeson (Value)
import Data.Time
import EulerHS.Prelude hiding (State)

data State = State
  { _descriptor :: Descriptor,
    _updated_at :: Maybe UTCTime,
    _updated_by :: Text,
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
        _updated_at = Nothing,
        _updated_by = "example",
        _update_metadata = Nothing
      }
