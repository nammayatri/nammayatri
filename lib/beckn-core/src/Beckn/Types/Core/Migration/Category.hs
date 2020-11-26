{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Category (Category (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Category = Category
  { _id :: Maybe Text,
    _parent_category_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _time :: Maybe Time,
    _tags :: Maybe [Tags] -- FIXME: probably needs to be just Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Category 'stripLensPrefixOptions
