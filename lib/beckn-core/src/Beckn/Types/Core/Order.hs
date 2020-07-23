{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Order where

import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Text,
    _items :: [Text],
    _created_at :: LocalTime,
    _updated_at :: LocalTime
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
        _state = "State",
        _items = [idExample],
        _created_at = example,
        _updated_at = example
      }
