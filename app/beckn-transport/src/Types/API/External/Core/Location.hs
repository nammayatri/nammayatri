module Types.API.External.Core.Location where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Scalar

data Location =
  Location
    { _type :: Text --""gps","address","station_code","area_code","city","country","circle","polygon","3dspace"
    , _gps :: Maybe GPS
    , _address :: Maybe Address
    , _station_code :: Maybe Text
    , _area_code :: Maybe Text
    , _city :: Maybe City
    , _country :: Maybe Country
    , _circle :: Maybe Circle
    , _polygon :: Maybe Text
    , _3dspace :: Maybe Text
    }
      deriving (Generic, Show)

instance FromJSON Location where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Location where
  toJSON = genericToJSON stripAllLensPrefixOptions

data GPS =
  GPS
    { _lat :: Text
    , _lon :: Text
    }
      deriving (Generic, Show)

instance FromJSON GPS where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON GPS where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Address =
  Address
    { _door :: Text
    , _building :: Text
    , _street :: Text
    , _area :: Text
    , _city :: Text
    , _country :: Text
    , _area_code :: Text
    }
      deriving (Generic, Show)

instance FromJSON Address where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Address where
  toJSON = genericToJSON stripAllLensPrefixOptions

data City =
  City
    { _name :: Text
    , _code :: Text
    }
      deriving (Generic, Show)

instance FromJSON City where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON City where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Country =
  Country
    { _name :: Text
    , _code :: Text
    }
      deriving (Generic, Show)

instance FromJSON Country where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Country where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Circle =
  Circle
    { _lat :: Text
    , _long :: Text
    , _radius :: Scalar
    }
      deriving (Generic, Show)

instance FromJSON Circle where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Circle where
  toJSON = genericToJSON stripAllLensPrefixOptions
