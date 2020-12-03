{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Ack where

import EulerHS.Prelude

data Status = ACK | NACK
  deriving (Generic, Show, FromJSON, ToJSON)

data Ack = Ack
  { _status :: Maybe Status,
    _signature :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripLensPrefixOptions
