{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Quotation where

import Beckn.Types.Core.Migration.Duration
import Beckn.Types.Core.Migration.Price
import Beckn.Utils.JSON (constructorsWithHyphensToLowerOptions)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Quotation = Quotation
  { _price :: Maybe Price,
    _breakup :: [BreakupItem],
    _ttl :: Maybe Duration
  }
  deriving (Generic, Show)

data BreakupItem = BreakupItem
  { _type :: Maybe BreakupItemType,
    _ref_id :: Maybe Text,
    _title :: Maybe Text,
    _price :: Maybe Price
  }
  deriving (Generic, Show)

data BreakupItemType
  = ITEM
  | OFFER
  | ADD_ON
  | FULFILLMENT
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Quotation
deriveJSON stripAllLensPrefixOptions ''BreakupItem
deriveJSON constructorsWithHyphensToLowerOptions ''BreakupItemType
