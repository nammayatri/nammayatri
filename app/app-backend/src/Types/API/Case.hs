module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Swagger hiding (info)
import Data.Time
import EulerHS.Prelude hiding (id, product)
import Types.Storage.Case
import Types.Storage.Location
import Types.Storage.Organization (Organization)
import Types.Storage.Person
import Types.Storage.ProductInstance
import Types.Storage.Products

data GetStatusRes = GetStatusRes
  { _case :: Case,
    productInstance :: [ProdInstRes],
    fromLocation :: Location,
    toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON GetStatusRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON GetStatusRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data UpdateCaseReq = UpdateCaseReq
  { quote :: Maybe Amount,
    transporterChoice :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data CaseRes = CaseRes
  { _case :: Case,
    productInstance :: [ProdInstRes],
    fromLocation :: Maybe Location,
    toLocation :: Maybe Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CaseRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type CaseListRes = [CaseRes]

data ProdInstRes = ProdInstRes
  { id :: Id ProductInstance,
    caseId :: Id Case,
    productId :: Id Products,
    personId :: Maybe (Id Person),
    shortId :: ShortId ProductInstance,
    entityType :: EntityType,
    entityId :: Maybe Text,
    quantity :: Int,
    price :: Maybe Amount,
    status :: ProductInstanceStatus,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    validTill :: UTCTime,
    fromLocation :: Maybe (Id Location),
    toLocation :: Maybe (Id Location),
    organizationId :: Id Organization,
    parentId :: Maybe (Id ProductInstance),
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    info :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    product :: Maybe Products
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data CaseInfo = CaseInfo
  { total :: Maybe Integer,
    accepted :: Maybe Integer,
    declined :: Maybe Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
