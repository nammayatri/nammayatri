module Epass.Types.API.Pass where

import Data.Swagger
import Data.Time.LocalTime
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Comment as SCM
import qualified Epass.Types.Storage.Customer as SC
import qualified Epass.Types.Storage.Document as SD
import qualified Epass.Types.Storage.Organization as SO
import Epass.Types.Storage.Pass
import qualified Epass.Types.Storage.Tag as ST
import EulerHS.Prelude

data PassRes = PassRes
  { _pass :: PassInfo
  }
  deriving (Generic, ToSchema)

data UpdatePassReq = UpdatePassReq
  { _action :: Maybe Status,
    _CustomerId :: Maybe CustomerId,
    _fromLocation :: Maybe Location,
    _toLocation :: Maybe Location
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdatePassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassReq = ListPassReq
  { _identifierType :: PassIDType,
    _identifier :: Text,
    _limit :: Int,
    _offset :: Int,
    __type :: PassType
  }
  deriving (Generic, ToSchema)

instance FromJSON ListPassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassRes = ListPassRes
  { passes :: [PassInfo]
  }
  deriving (Generic, ToJSON, ToSchema)

data PassInfo = PassInfo
  { _id :: PassId,
    _ShortId :: Text,
    _TenantOrganizationId :: (Maybe TenantOrganizationId),
    _status :: Status,
    _fromDate :: LocalTime,
    _toDate :: LocalTime,
    _passType :: PassType,
    _PassApplicationId :: PassApplicationId,
    _CreatedBy :: CustomerId,
    _info :: Text,
    _createdAt :: LocalTime,
    _updatedAt :: LocalTime,
    _fromLocation :: Location,
    _toLocation :: Location,
    _Organization :: (Maybe SO.Organization),
    _Customer :: (Maybe SC.Customer),
    _Comments :: [SCM.Comment],
    _Tags :: [ST.Tag],
    _Documents :: [SD.Document]
  }
  deriving (Generic, ToSchema)

instance ToJSON PassInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance ToJSON PassRes where
  toJSON = genericToJSON stripLensPrefixOptions
