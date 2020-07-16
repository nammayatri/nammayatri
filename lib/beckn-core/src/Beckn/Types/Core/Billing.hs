{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Billing where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import EulerHS.Prelude

data Billing = Billing
  { _person :: Person,
    _location :: Location,
    _form :: Text
  }
  deriving (Generic, Show)

instance FromJSON Billing where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Billing where
  toJSON = genericToJSON stripAllLensPrefixOptions
