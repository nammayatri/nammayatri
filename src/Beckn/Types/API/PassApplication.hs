module Beckn.Types.API.PassApplication where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Comment         as SCM
import qualified Beckn.Types.Storage.Customer        as SC
import qualified Beckn.Types.Storage.Document        as SD
import qualified Beckn.Types.Storage.Organization    as SO
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.Tag             as ST

import           Data.Default
import           Data.Swagger
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreatePassApplicationReq =
  CreatePassApplicationReq
    { _CustomerId           :: Maybe CustomerId
    , _OrganizationId       :: Maybe OrganizationId
    , _TenantOrganizationId :: Maybe TenantOrganizationId
    , _fromDate             :: LocalTime
    , _toDate               :: LocalTime
    , _fromLocation         :: Maybe Location
    , _toLocation           :: Location
    , _purpose              :: Maybe Text
    , _travellerName        :: Maybe Text
    , _travellerID          :: Maybe Text
    , _travellerIDType      :: Maybe TravellerIDType
    , _type                 :: PassApplicationType
    , _count                :: Maybe Int
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
    { _passApplications :: [PassAppInfo]
    }
  deriving (Generic, ToSchema)

data PassAppInfo =
  PassAppInfo
    { _id                        :: PassApplicationId
    , _Customer                  :: (Maybe SC.Customer)
    , _Tags                      :: [ST.Tag]
    , _Documents                 :: [SD.Document]
    , _Comments                  :: [SCM.Comment]
    , _Organization              :: (Maybe SO.Organization)
    , _isBlacklistedOrganization :: Bool
    , _isBlacklistedLocation     :: Bool
    , _TenantOrganizationId      :: (Maybe TenantOrganizationId)
    , _status                    :: Status
    , _fromDate                  :: LocalTime
    , _toDate                    :: LocalTime
    , _passType                  :: PassType
    , _purpose                   :: (Maybe Text)
    , _fromLocation              :: Maybe Location
    , _toLocation                :: Location
    , _CreatedBy                 :: CustomerId
    , _AssignedTo                :: UserId
    , _count                     :: Int
    , _approvedCount             :: Int
    , _remarks                   :: Text
    , _info                      :: Text
    , _createdAt                 :: LocalTime
    , _updatedAt                 :: LocalTime
    }
      deriving (Generic, ToSchema)

instance ToJSON ListPassApplicationRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance ToJSON PassAppInfo where
  toJSON = genericToJSON stripLensPrefixOptions


data UpdatePassApplicationReq =
  UpdatePassApplicationReq
    { _status        :: Status
    , _approvedCount :: Maybe Int
    , _remarks       :: Text
    } deriving (Generic, ToSchema)

instance FromJSON UpdatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
