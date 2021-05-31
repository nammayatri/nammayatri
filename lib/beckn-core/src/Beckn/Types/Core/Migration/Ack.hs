{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Ack where

import EulerHS.Prelude

data Status = ACK | NACK
  deriving (Generic, Show, FromJSON, ToJSON)

newtype Ack = Ack
  { _status :: Status
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripLensPrefixOptions
