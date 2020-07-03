{-# LANGUAGE DeriveAnyClass #-}

module Beckn.External.Graphhopper.Types where

import Data.Aeson
import Data.Geospatial
import EulerHS.Prelude hiding (Show)
import Prelude (Show (..))

data Weighting = FASTEST | SHORTEST | SHORT_FASTEST
  deriving (Show)

data Vehicle = CAR | BIKE | FOOT | HIKE | MTB | RACINGBIKE | SCOOTER | TRUCK | SMALL_TRUCK
  deriving (Show)

data Request = Request
  { _points' :: [PointXY],
    _vehicle :: Vehicle,
    _weighting :: Maybe Weighting,
    _elevation :: Maybe Bool,
    _calcPoints :: Maybe Bool
  }
  deriving (Show)

data Path = Path
  { _distance :: Float, -- meters
    _time :: Integer, -- miliseconds
    _bbox :: Maybe BoundingBoxWithoutCRS, -- bbox and points fields are empty incase calcPoints
    _points :: Maybe GeospatialGeometry, -- is set to False. Default - True
    _snapped_waypoints :: GeospatialGeometry,
    _transfers :: Integer,
    _instructions :: Maybe [Instruction]
  }
  deriving (Generic, Show)

instance FromJSON Path where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Path where
  toJSON = genericToJSON stripLensPrefixOptions

data Instruction = Instruction
  { _distance :: Float,
    _heading :: Maybe Float,
    _sign :: Integer,
    _interval :: [Integer],
    _text :: String,
    _time :: Int,
    _street_name :: String
  }
  deriving (Generic, Show)

instance FromJSON Instruction where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Instruction where
  toJSON = genericToJSON stripLensPrefixOptions

data Response = Response
  { _paths :: [Path]
  }
  deriving (Generic, Show)

instance FromJSON Response where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Response where
  toJSON = genericToJSON stripLensPrefixOptions
