{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Ack where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Status = ACK | NACK
  deriving (Generic, Show, FromJSON, ToJSON)

data Ack = Ack
  { _status :: Maybe Status,
    _signature :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Ack
