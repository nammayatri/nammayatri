{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Option where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Option = Option
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show)

deriveJSON ''Option 'stripLensPrefixOptions
