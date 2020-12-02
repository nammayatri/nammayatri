{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Organization (Organization (..)) where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Organization = Organization
  { _name :: Maybe Text,
    _cred :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Organization
