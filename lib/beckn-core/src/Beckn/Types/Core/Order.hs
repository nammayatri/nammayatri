{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Fulfillment
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Text,
    _billing :: Maybe Billing,
    _fulfillment :: Maybe Fulfillment,
    _created_at :: LocalTime,
    _updated_at :: LocalTime
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions
