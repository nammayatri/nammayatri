module Beckn.Types.API.PassApplication where

import Beckn.Types.API.Common
import Beckn.Types.Storage.PassApplication
import Data.Default
import EulerHS.Prelude

data CreatePassApplicationReq =
  CreatePassApplicationReq
    { __CustomerId :: Text
    , _fromDate :: Text
    , _toDate :: Text
    , _fromLocation :: Maybe Text
    , _toLocation :: Text
    , _travellerName :: Maybe Text
    , _travellerID :: Maybe Text
    , _travellerIDType :: Maybe TravellerIDType
    , __type :: PassApplicationType
    }
  deriving (Generic)

instance FromJSON CreatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data PassApplicationRes =
  PassApplicationRes
    { passApplication :: PassApplication
    }
  deriving (Generic, ToJSON)

------ List Pass Application ------
data ListPassApplicationReq =
  ListPassApplicationReq
    { _limit :: Int
    , _offset :: Int
    , _status :: [Status] -- Empty for all kind of applications
    , __type :: [PassType]
    }
  deriving (Generic)

instance FromJSON ListPassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassApplicationRes =
  ListPassApplicationRes
    { passApplications :: [PassApplication]
    }
  deriving (Generic, ToJSON)

data UpdatePassApplicationReq =
  UpdatePassApplicationReq
    { _status :: Status
    , _approvedCount :: Int
    , _remarks :: Text
    } deriving (Generic)

instance FromJSON UpdatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
