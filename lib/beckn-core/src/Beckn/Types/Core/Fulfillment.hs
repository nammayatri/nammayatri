{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Fulfillment where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import EulerHS.Prelude

data Fulfillment = Fulfillment
  { _person :: Person,
    _location :: Location,
    _form :: Text
  }
  deriving (Generic, Show)

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripAllLensPrefixOptions
