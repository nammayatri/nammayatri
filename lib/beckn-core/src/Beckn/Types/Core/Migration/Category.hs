{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Category (Category (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Category = Category
  { _id :: Maybe Text,
    _parent_category_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _time :: Maybe Time,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Category
