{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Fulfillment where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import Beckn.Utils.JSON
import EulerHS.Prelude

data Fulfillment = Fulfillment
  { person :: Person,
    location :: Location,
    form :: Text
  }
  deriving (Generic, Show)

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
