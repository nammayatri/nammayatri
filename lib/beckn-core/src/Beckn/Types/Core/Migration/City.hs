{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.City (City (..)) where

import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data City = City
  { _name :: Maybe Text,
    _code :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON ''City 'stripAllLensPrefixOptions
