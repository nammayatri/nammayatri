module Types.API.Case where

import Beckn.Types.App
import Beckn.Types.Core.Amount
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Products
import Data.Swagger
import Data.Time
import EulerHS.Prelude

data StatusRes = StatusRes
  { _case :: Case,
    _productInstance :: [ProdInstRes],
    _fromLocation :: Location,
    _toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON StatusRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON StatusRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

data UpdateCaseReq = UpdateCaseReq
  { _quote :: Maybe Amount,
    _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data CaseRes = CaseRes
  { _case :: Case,
    _productInstance :: [ProdInstRes],
    _fromLocation :: Maybe Location,
    _toLocation :: Maybe Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type CaseListRes = [CaseRes]

data ProdInstRes = ProdInstRes
  { _id :: ProductInstanceId,
    _caseId :: CaseId,
    _productId :: ProductsId,
    _personId :: Maybe PersonId,
    _shortId :: Text,
    _entityType :: EntityType,
    _entityId :: Maybe Text,
    _quantity :: Int,
    _price :: Amount,
    _status :: ProductInstanceStatus,
    _startTime :: UTCTime,
    _endTime :: Maybe UTCTime,
    _validTill :: UTCTime,
    _fromLocation :: Maybe Text,
    _toLocation :: Maybe Text,
    _organizationId :: Text,
    _parentId :: Maybe ProductInstanceId,
    _udf1 :: Maybe Text,
    _udf2 :: Maybe Text,
    _udf3 :: Maybe Text,
    _udf4 :: Maybe Text,
    _udf5 :: Maybe Text,
    _info :: Maybe Text,
    _createdAt :: UTCTime,
    _updatedAt :: UTCTime,
    _product :: Maybe Products
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdInstRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdInstRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
