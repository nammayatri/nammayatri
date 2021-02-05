module Types.API.CustomerSupport where

import Beckn.Types.Amount
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Location as L
import Beckn.Types.Storage.Person as P
import Beckn.Types.Storage.ProductInstance as SP
import Data.Time
import EulerHS.Prelude
import Types.Common

newtype OrderResp = OrderResp {_order :: OrderDetails}
  deriving (Show, Generic)

instance FromJSON OrderResp where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OrderResp where
  toJSON = genericToJSON stripAllLensPrefixOptions

data OrderDetails
  = OrderDetails
      { _id :: Text,
        _status :: Maybe CaseStatus,
        _createdAt :: UTCTime,
        _updatedAt :: UTCTime,
        _startTime :: UTCTime,
        _endTime :: Maybe UTCTime,
        _fromLocation :: Maybe L.Location,
        _toLocation :: Maybe L.Location,
        _vehicleVariant :: Maybe Text,
        _travellerName :: Maybe Text,
        _travellerPhone :: Maybe Text,
        _trip :: Maybe TripDetails
      }
  deriving (Show, Generic)

instance FromJSON OrderDetails where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OrderDetails where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TripDetails
  = TripDetails
      { _id :: Text, -- Product Instance ID
        _status :: SP.ProductInstanceStatus,
        _driver :: Maybe Driver, -- _info -> driver
        _vehicle :: Maybe Vehicle,
        _provider :: Maybe Provider,
        _price :: Amount
      }
  deriving (Show, Generic)

instance FromJSON TripDetails where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TripDetails where
  toJSON = genericToJSON stripAllLensPrefixOptions

data OrderInfo
  = OrderInfo
      { person :: P.Person,
        searchcases :: [C.Case]
      }
  deriving (Show, Generic)

data LoginReq
  = LoginReq
      { _email :: Text,
        _password :: Text
      }
  deriving (Show, Generic)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LoginReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data LoginRes
  = LoginRes
      { _auth_token :: Text,
        _message :: Text
      }
  deriving (Show, Generic)

instance FromJSON LoginRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LoginRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
