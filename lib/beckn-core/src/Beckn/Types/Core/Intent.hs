{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Intent where

import Beckn.Types.Core.Tag
import Data.Text
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider_id :: Maybe Text,
    _category_id :: Maybe Text,
    _item_id :: Maybe Text,
    _tags :: [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripAllLensPrefixOptions
