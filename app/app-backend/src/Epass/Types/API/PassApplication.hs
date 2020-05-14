module Epass.Types.API.PassApplication where

import qualified Beckn.Types.Storage.Case            as Case
import qualified Beckn.Types.Storage.Location        as Loc
import           Data.Default
import           Data.Swagger
import           Data.Time.LocalTime
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Storage.Comment         as SCM
import qualified Epass.Types.Storage.Customer        as SC
import qualified Epass.Types.Storage.Document        as SD
import qualified Epass.Types.Storage.Organization    as SO
import           Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.Tag             as ST
import           EulerHS.Prelude

data CreatePassApplicationReq = CreatePassApplicationReq
  { _CustomerId           :: Maybe PersonId,
    _OrganizationId       :: Maybe OrganizationId,
    _TenantOrganizationId :: Maybe TenantOrganizationId,
    _fromDate             :: LocalTime,
    _toDate               :: LocalTime,
    _fromLocation         :: Maybe Location,
    _toLocation           :: Location,
    _purpose              :: Maybe Text,
    _travellerName        :: Maybe Text,
    _travellerID          :: Maybe Text,
    _travellerIDType      :: Maybe TravellerIDType,
    _type                 :: PassApplicationType,
    _count                :: Maybe Int
  }
  deriving (Generic, ToSchema)

instance FromJSON CreatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data PassApplicationRes' = PassApplicationRes'
  { passApplication :: Case.Case
  }
  deriving (Generic, ToJSON, ToSchema)


data PassApplicationRes = PassApplicationRes
  { passApplication :: PassApplication
  }
  deriving (Generic, ToJSON, ToSchema)

data GetPassApplication = GetPassApplication
  { passApplication :: PassAppInfo
  }
  deriving (Generic, ToJSON, ToSchema)

------ List Pass Application ------
data ListPassApplicationRes = ListPassApplicationRes
  { _passApplications :: [CaseInfo] --TODO: embed all pass appinfo entities (docs, tags, comments)
  }
  deriving (Generic, ToSchema)

data PassAppInfo = PassAppInfo
  { _id                        :: PassApplicationId,
    _Customer                  :: (Maybe SC.Customer),
    _Tags                      :: [ST.Tag],
    _Documents                 :: [SD.Document],
    _Comments                  :: [SCM.Comment],
    _Organization              :: (Maybe SO.Organization),
    _isBlacklistedOrganization :: Bool,
    _isBlacklistedLocation     :: Bool,
    _TenantOrganizationId      :: (Maybe TenantOrganizationId),
    _status                    :: Status,
    _fromDate                  :: LocalTime,
    _toDate                    :: LocalTime,
    _passType                  :: PassType,
    _purpose                   :: (Maybe Text),
    _fromLocation              :: Maybe Location,
    _toLocation                :: Location,
    _CreatedBy                 :: CustomerId,
    _AssignedTo                :: UserId,
    _count                     :: Int,
    _approvedCount             :: Int,
    _remarks                   :: Text,
    _info                      :: Text,
    _createdAt                 :: LocalTime,
    _updatedAt                 :: LocalTime
  }
  deriving (Generic, ToSchema)

instance ToJSON ListPassApplicationRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance ToJSON PassAppInfo where
  toJSON = genericToJSON stripLensPrefixOptions


data CaseInfo = CaseInfo
  { _id                        :: CaseId,
    _Customer                  :: (Maybe SC.Customer),
    _Tags                      :: [ST.Tag],
    _Documents                 :: [SD.Document],
    _Comments                  :: [SCM.Comment],
    _Organization              :: (Maybe SO.Organization),
    _isBlacklistedOrganization :: Bool,
    _isBlacklistedLocation     :: Bool,
    _TenantOrganizationId      :: (Maybe TenantOrganizationId),
    _status                    :: Case.CaseStatus,
    _fromDate                  :: LocalTime,
    _toDate                    :: Maybe LocalTime,
    _passType                  :: Maybe Text, -- PassType,
    _purpose                   :: (Maybe Text),
    _fromLocation              :: Maybe Loc.Location,
    _toLocation                :: Maybe Loc.Location,
    _CreatedBy                 :: Maybe Text, -- CustomerId,
    _AssignedTo                :: Maybe UserId,
    _count                     :: Maybe Text, -- Int,
    _approvedCount             :: Maybe Text, -- Int,
    _remarks                   :: Maybe Text,
    _info                      :: Maybe Text,
    _createdAt                 :: LocalTime,
    _updatedAt                 :: LocalTime
  }
  deriving (Generic, ToSchema)

instance ToJSON CaseInfo where
  toJSON = genericToJSON stripLensPrefixOptions

data UpdatePassApplicationReq = UpdatePassApplicationReq
  { _status        :: Status,
    _approvedCount :: Maybe Int,
    _remarks       :: Maybe Text
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdatePassApplicationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
