{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Ack where

import EulerHS.Prelude

data Status = ACK | NACK
  deriving (Generic, Show, FromJSON, ToJSON)

newtype Ack = Ack
  { status :: Status
  }
  deriving (Generic, FromJSON, ToJSON, Show)
