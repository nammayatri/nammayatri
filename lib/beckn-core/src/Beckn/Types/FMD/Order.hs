module Beckn.Types.FMD.Order where

import Beckn.Types.FMD.Task (Task)
import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Text,
    _items :: [Text],
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _tasks :: [Task]
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Order where
  example =
    Order
      { _id = idExample,
        _state = "READY",
        _items = [idExample],
        _created_at = example,
        _updated_at = example,
        _tasks = example
      }
