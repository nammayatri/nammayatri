{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Country (Country (..)) where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Country = Country
  { _name :: Maybe Text,
    _code :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Country
