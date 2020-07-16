{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Fulfillment
import Beckn.Types.Mobility.Trip
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Maybe Text,
    _billing :: Maybe Billing,
    _fulfillment :: Maybe Fulfillment,
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _trip :: Maybe Trip,
    _invoice :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions
