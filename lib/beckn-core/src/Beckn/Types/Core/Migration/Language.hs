{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Language where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

newtype Language = Language
  { _code :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Language
