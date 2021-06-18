module Types.API.CustomerSupport where

import Beckn.Types.Amount
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Location as L
import qualified Beckn.Types.Storage.Person as P
import Beckn.Types.Storage.ProductInstance as SP
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude hiding (id)
import Types.Common

newtype OrderResp = OrderResp {_order :: OrderDetails}
  deriving (Show, Generic)

instance FromJSON OrderResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON OrderResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data OrderDetails = OrderDetails
  { id :: Text,
    status :: Maybe CaseStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    fromLocation :: Maybe L.Location,
    toLocation :: Maybe L.Location,
    vehicleVariant :: Maybe Text,
    travellerName :: Maybe Text,
    travellerPhone :: Maybe Text,
    trip :: Maybe TripDetails
  }
  deriving (Show, Generic)

instance FromJSON OrderDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON OrderDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data TripDetails = TripDetails
  { id :: Text, -- Product Instance Id
    status :: SP.ProductInstanceStatus,
    driver :: Maybe Driver, -- info -> driver
    vehicle :: Maybe Vehicle,
    provider :: Maybe Provider,
    price :: Maybe Amount
  }
  deriving (Show, Generic)

instance FromJSON TripDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TripDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data OrderInfo = OrderInfo
  { person :: P.Person,
    searchcases :: [C.Case]
  }
  deriving (Generic)

data LoginReq = LoginReq
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LoginRes = LoginRes
  { auth_token :: Text,
    message :: Text
  }
  deriving (Show, Generic)

instance FromJSON LoginRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LoginRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic)

instance FromJSON LogoutRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LogoutRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
