{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Language where

import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

newtype Language = Language
  { _code :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON ''Language 'stripLensPrefixOptions
