{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Intent where

import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.Core.Tag
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Data.Text
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider_id :: Maybe Text,
    _category_id :: Maybe Text,
    _item_id :: Maybe Text,
    _tags :: [Tag],
    -- Mobility specific
    _origin :: Stop,
    _destination :: Stop,
    _stops :: [Stop],
    _vehicle :: Vehicle,
    _payload :: Payload,
    _transfer :: Maybe TransferAttrs,
    _fare :: Price
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TransferAttrs = TransferAttrs
  { _max_count :: Int,
    _distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON TransferAttrs where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON TransferAttrs where
  toJSON = genericToJSON stripAllLensPrefixOptions
