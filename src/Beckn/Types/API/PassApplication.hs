module Beckn.Types.API.PassApplication where

import Beckn.Types.API.Common
import Beckn.Types.Storage.PassApplication
import Data.Default
import EulerHS.Prelude
import Data.Swagger

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
  deriving (Generic, ToSchema)

instance FromJSON CreatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data PassApplicationRes =
  PassApplicationRes
    { passApplication :: PassApplication
    }
  deriving (Generic, ToJSON, ToSchema)

------ List Pass Application ------
data ListPassApplicationRes =
  ListPassApplicationRes
    { passApplications :: [PassApplication]
    }
  deriving (Generic, ToJSON, ToSchema)

data UpdatePassApplicationReq =
  UpdatePassApplicationReq
    { _status :: Status
    , _approvedCount :: Int
    , _remarks :: Text
    } deriving (Generic, ToSchema)

instance FromJSON UpdatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
