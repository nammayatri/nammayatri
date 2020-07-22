{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Billing where

import Beckn.Types.Core.Address
import Beckn.Types.Core.Person
import EulerHS.Prelude

data Billing = Billing
  { _customer :: Person,
    _address :: Address,
    _period :: Text -- Describes duration as per ISO8601 format
  }
  deriving (Generic, Show)

instance FromJSON Billing where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Billing where
  toJSON = genericToJSON stripAllLensPrefixOptions
