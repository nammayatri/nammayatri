{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.MonetaryValue (MonetaryValue (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { _currency :: Maybe Text,
    _value :: Maybe DecimalValue
  }
  deriving (Eq, Generic, Show)

deriveJSON stripAllLensPrefixOptions ''MonetaryValue
