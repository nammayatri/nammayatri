{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Provider (Provider (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Provider = Provider
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _time :: Maybe Time,
    _locations :: Maybe [Location],
    _tags :: Maybe [Tags] -- FIXME: probably needs to be just Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Provider 'stripAllLensPrefixOptions
