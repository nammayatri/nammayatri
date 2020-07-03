module Epass.Types.API.Pass where

import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance
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

newtype PassRes = PassRes
  { _pass :: PassInfo
  }
  deriving (Generic, FromJSON, ToSchema)

data UpdatePassReq = UpdatePassReq
  { _action :: Maybe ProductInstanceStatus,
    _CustomerId :: Maybe CustomerId,
    _fromLocation :: Maybe Text,
    _toLocation :: Maybe Text
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdatePassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdatePassReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

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

newtype ListPassRes = ListPassRes
  { passes :: [PassInfo]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data PassInfo = PassInfo
  { _id :: Text,
    _ShortId :: Text,
    _TenantOrganizationId :: Maybe TenantOrganizationId,
    _status :: ProductInstanceStatus,
    _fromDate :: LocalTime,
    _toDate :: LocalTime,
    _passType :: PassType,
    _PassApplicationId :: Text,
    _CreatedBy :: Text,
    --_info :: Text,
    --_createdAt :: LocalTime,
    --_updatedAt :: LocalTime,
    _fromLocation :: Text, -- Location,
    _toLocation :: Text, -- Location,
    _Organization :: Maybe SO.Organization,
    _Customer :: Maybe SP.Person,
    --_Comments :: [SCM.Comment],
    --_Tags :: [ST.Tag],
    _Documents :: [SD.Document]
  }
  deriving (Generic, ToSchema)

instance ToJSON PassInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON PassInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON PassRes where
  toJSON = genericToJSON stripLensPrefixOptions
