{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Item where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Item = Item
  { _id :: Maybe Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price,
    _category_id :: Maybe Text,
    _location_id :: Maybe Text,
    _time :: Maybe Time,
    _tags :: Maybe [Tags] -- FIXME: probably needs to be just Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Item 'stripAllLensPrefixOptions
