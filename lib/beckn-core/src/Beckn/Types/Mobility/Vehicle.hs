module Beckn.Types.Mobility.Vehicle where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude

data Vehicle =
  Vehicle
    { _category :: Text -- "CAR", "MOTORCYCLE", "BICYCLE", "TRUCK", "OTHER"
    , _capaciity :: Int
    , _make :: Text
    , _model :: Text
    , _size :: Text
    , _variant :: Text
    , _color :: Text
    , _energy_type :: Text -- "PETROL", "DIESEL", "LPG", "CNG", "EV", "OTHER"
    , _registration :: Registration
    }
      deriving (Generic, Show)

instance FromJSON Vehicle where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Vehicle where
  toJSON = genericToJSON stripLensPrefixOptions

data Registration =
  Registration
    { _category :: Text -- "PERSONAL", "COMMERCIAL", "OTHER"
    , _number :: Text
    }
      deriving (Generic, Show)

instance FromJSON Registration where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Registration where
  toJSON = genericToJSON stripLensPrefixOptions