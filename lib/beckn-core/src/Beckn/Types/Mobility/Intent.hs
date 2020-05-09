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
  { _domain :: Text,
    _origin :: Location,
    _destination :: Location,
    _time :: LocalTime,
    _stops :: [Stop],
    _vehicle :: Vehicle,
    _providers :: [Provider],
    _payload :: Payload,
    _transfer_attrs :: TransferAttrs,
    _fare_range :: ScalarRange,
    _tags :: [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripLensPrefixOptions

data TransferAttrs = TransferAttrs
  { _max_count :: Int,
    _max_distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON TransferAttrs where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TransferAttrs where
  toJSON = genericToJSON stripLensPrefixOptions

data Payload = Payload
  { _travellers :: TravellerReqInfo,
    _luggage :: Luggage
  }
  deriving (Generic, Show)

instance FromJSON Payload where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Payload where
  toJSON = genericToJSON stripLensPrefixOptions

data Luggage = Luggage
  { _count :: Int,
    _weight_range :: ScalarRange,
    _dimensions :: Dimension
  }
  deriving (Generic, Show)

instance FromJSON Luggage where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Luggage where
  toJSON = genericToJSON stripLensPrefixOptions

data Dimension = Dimension
  { _length :: Scalar,
    _breadth :: Scalar,
    _height :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimension where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Dimension where
  toJSON = genericToJSON stripLensPrefixOptions

data TravellerReqInfo = TravellerReqInfo
  { _count :: Int
  }
  deriving (Generic, Show)

instance FromJSON TravellerReqInfo where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TravellerReqInfo where
  toJSON = genericToJSON stripLensPrefixOptions
