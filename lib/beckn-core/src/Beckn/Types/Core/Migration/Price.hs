{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Price (Price (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

-- allOf case
data Price = Price
  { _currency :: Maybe Text,
    _value :: Maybe DecimalValue,
    _estimated_value :: Maybe DecimalValue,
    _computed_value :: Maybe DecimalValue,
    _listed_value :: Maybe DecimalValue,
    _offered_value :: Maybe DecimalValue,
    _minimum_value :: Maybe DecimalValue,
    _maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Price
