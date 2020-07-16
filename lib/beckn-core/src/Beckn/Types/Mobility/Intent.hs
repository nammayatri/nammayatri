{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Intent where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Scalar
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Core.Tag
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Data.Text
import Data.Time
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider_id :: Maybe Text,
    _category_id :: Maybe Text,
    _item_id :: Maybe Text,
    _tags :: [Tag],
    _origin :: Stop,
    _destination :: Stop,
    _stops :: [Stop],
    _vehicle :: Vehicle,
    _payload :: Payload,
    _transfer_attrs :: Maybe TransferAttrs,
    _fare_range :: ScalarRange
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TransferAttrs = TransferAttrs
  { _max_count :: Int,
    _max_distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON TransferAttrs where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON TransferAttrs where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Payload = Payload
  { _travellers :: TravellerReqInfo,
    _luggage :: Luggage
  }
  deriving (Generic, Show)

instance FromJSON Payload where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Payload where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Luggage = Luggage
  { _count :: Int,
    _weight_range :: Maybe ScalarRange,
    _dimensions :: Maybe Dimension
  }
  deriving (Generic, Show)

instance FromJSON Luggage where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Luggage where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Dimension = Dimension
  { _length :: Scalar,
    _breadth :: Scalar,
    _height :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimension where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Dimension where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype TravellerReqInfo = TravellerReqInfo
  { _count :: Int
  }
  deriving (Generic, Show)

instance FromJSON TravellerReqInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON TravellerReqInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions
