module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Products
import Beckn.Utils.JSON
import Data.Swagger
import Data.Time
import EulerHS.Prelude

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
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdInstRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProdInstRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data CaseInfo = CaseInfo
  { total :: Maybe Integer,
    accepted :: Maybe Integer,
    declined :: Maybe Integer
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CaseInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
