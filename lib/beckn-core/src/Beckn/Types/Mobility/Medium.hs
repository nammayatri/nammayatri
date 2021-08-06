module Beckn.Types.Mobility.Medium where

import Beckn.Types.Core.Price
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Medium = Medium
  { _type :: Text, -- "ROADWAY", "WATERWAY", "AIRWAY", "RAILWAY"
    roadway :: Maybe Roadway,
    waterway :: Maybe Waterway,
    airway :: Maybe Airway,
    railway :: Maybe Railway
  }
  deriving (Generic, Show)

instance FromJSON Medium where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Medium where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Roadway = Roadway
  { _type :: Text, --"HIGHWAY", "LOCAL-ROAD"
    lanes :: Text,
    oneway :: Bool,
    toll :: Toll
  }
  deriving (Generic, Show)

instance FromJSON Roadway where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Roadway where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Toll = Toll
  { has_toll :: Bool,
    price :: Price --not available in mobility in github, so taking from core.
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Waterway = Waterway
  { _type :: Text --"SEA", "RIVER", "LAKE"
  }
  deriving (Generic, Show)

instance FromJSON Waterway where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Waterway where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype Airway = Airway
  { _type :: Text --"CIVILIAN", "MILITARY"
  }
  deriving (Generic, Show)

instance FromJSON Airway where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Airway where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype Railway = Railway
  { _type :: Text --"NARROW-GAUGE", "METER-GAUGE", "BROAD-GAUGE"
  }
  deriving (Generic, Show)

instance FromJSON Railway where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Railway where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
