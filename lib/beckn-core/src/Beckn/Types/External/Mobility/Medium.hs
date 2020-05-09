module Beckn.Types.External.Mobility.Medium where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Price

data Medium =
  Medium
    { _type :: Text -- "ROADWAY", "WATERWAY", "AIRWAY", "RAILWAY"
    , _roadway :: Maybe Roadway
    , _waterway :: Maybe Waterway
    , _airway :: Maybe Airway
    , _railway :: Maybe Railway
    }
      deriving (Generic, Show)

instance FromJSON Medium where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Medium where
  toJSON = genericToJSON stripLensPrefixOptions

data Roadway =
  Roadway
    { _type :: Text --"HIGHWAY", "LOCAL-ROAD"
    , _lanes :: Text
    , _oneway :: Bool
    , _toll :: Toll
    }
      deriving (Generic, Show)

instance FromJSON Roadway where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Roadway where
  toJSON = genericToJSON stripLensPrefixOptions

data Toll =
  Toll
    { _has_toll :: Bool
    , _price :: Price --not available in mobility in github, so taking from core.
    }
      deriving (Generic, Show)

instance FromJSON Toll where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Toll where
  toJSON = genericToJSON stripLensPrefixOptions

data Waterway =
  Waterway
    { _type :: Text --"SEA", "RIVER", "LAKE"
    }
      deriving (Generic, Show)

instance FromJSON Waterway where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Waterway where
  toJSON = genericToJSON stripLensPrefixOptions

data Airway =
  Airway
    { _type :: Text --"CIVILIAN", "MILITARY"
    }
      deriving (Generic, Show)

instance FromJSON Airway where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Airway where
  toJSON = genericToJSON stripLensPrefixOptions

data Railway =
  Railway
    { _type :: Text --"NARROW-GAUGE", "METER-GAUGE", "BROAD-GAUGE"
    }
      deriving (Generic, Show)

instance FromJSON Railway where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Railway where
  toJSON = genericToJSON stripLensPrefixOptions
