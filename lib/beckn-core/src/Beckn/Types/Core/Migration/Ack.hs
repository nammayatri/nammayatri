{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Ack where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Status = ACK | NACK
  deriving (Generic, Show, FromJSON, ToJSON)

newtype Ack = Ack
  { status :: Status
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Ack where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
