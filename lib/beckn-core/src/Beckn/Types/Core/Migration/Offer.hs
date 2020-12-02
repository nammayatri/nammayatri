{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Offer (Offer (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Time (Time)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Offer = Offer
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _location_ids :: [Text],
    _category_ids :: [Text],
    _item_ids :: [Text],
    _time :: Maybe Time
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Offer
