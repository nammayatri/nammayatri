module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Char (toLower)
import Data.Swagger hiding (info)
import Data.Time
import EulerHS.Prelude hiding (id, product)
import Types.API.MetroOffer (MetroOffer (..))
import Types.Storage.Case
import Types.Storage.Organization (Organization)
import Types.Storage.Person
import Types.Storage.ProductInstance
import Types.Storage.Products
import Types.Storage.SearchReqLocation

data GetStatusRes = GetStatusRes
  { _case :: Case,
    productInstance :: [OfferRes],
    fromLocation :: SearchReqLocation,
    toLocation :: SearchReqLocation
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
    productInstance :: [OfferRes],
    fromLocation :: Maybe SearchReqLocation,
    toLocation :: Maybe SearchReqLocation
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CaseRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data OfferRes
  = OnDemandCab ProdInstRes
  | Metro MetroOffer
  deriving (Show, Generic, ToSchema)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

-- either:
-- {"onDemandCab": ...old type}
-- or
-- {"metro": ...new MetroOffer type}

creationTime :: OfferRes -> UTCTime
creationTime (OnDemandCab ProdInstRes {createdAt}) = createdAt
creationTime (Metro MetroOffer {createdAt}) = createdAt

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
    estimatedFare :: Maybe Amount,
    fare :: Maybe Amount,
    estimatedTotalFare :: Maybe Amount,
    totalFare :: Maybe Amount,
    discount :: Maybe Amount,
    status :: ProductInstanceStatus,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    validTill :: UTCTime,
    fromLocation :: Maybe (Id SearchReqLocation),
    toLocation :: Maybe (Id SearchReqLocation),
    organizationId :: Id Organization,
    parentId :: Maybe (Id ProductInstance),
    chargeableDistance :: Maybe Double,
    vehicleVariant :: Text,
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
