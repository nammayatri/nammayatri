{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Offer (Offer (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Offer = Offer
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _location_ids :: Maybe [Text],
    _category_ids :: Maybe [Text],
    _item_ids :: Maybe [Text],
    _time :: Maybe Time
  }
  deriving (Generic, Show)

deriveJSON ''Offer 'stripLensPrefixOptions
