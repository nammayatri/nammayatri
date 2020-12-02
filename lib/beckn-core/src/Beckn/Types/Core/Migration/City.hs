{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.City (City (..)) where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data City = City
  { _name :: Maybe Text,
    _code :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''City
