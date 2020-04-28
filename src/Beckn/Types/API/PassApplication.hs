module Beckn.Types.API.PassApplication where

import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.PassApplication
import           Data.Default
import           Data.Swagger
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreatePassApplicationReq =
  CreatePassApplicationReq
    { _CustomerId      :: CustomerId
    , _fromDate        :: LocalTime
    , _toDate          :: LocalTime
    , _fromLocation    :: Maybe Location
    , _toLocation      :: Location
    , _travellerName   :: Maybe Text
    , _travellerID     :: Maybe Text
    , _travellerIDType :: Maybe TravellerIDType
    , _type            :: PassApplicationType
    , _count           :: Maybe Int
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
    { _status        :: Status
    , _approvedCount :: Int
    , _remarks       :: Text
    } deriving (Generic, ToSchema)

instance FromJSON UpdatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
