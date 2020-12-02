{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.AddOn (AddOn (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data AddOn = AddOn
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''AddOn
