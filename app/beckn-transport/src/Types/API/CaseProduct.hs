module Types.API.CaseProduct where

import           Data.Default
import           Data.Swagger
import           Data.Time
import           Beckn.Types.Storage.CaseProduct
import qualified Beckn.Types.Storage.Products   as Product
import qualified Beckn.Types.Storage.Case       as Case
import qualified Beckn.Types.Storage.Location as Loc
import           EulerHS.Prelude

data CaseProdReq = CaseProdReq
  { _type   :: Product.ProductsStatus,
    _limit :: Integer,
    _offset :: Integer,
    _fromTime :: LocalTime,
    _toTime  :: LocalTime
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions


data CaseProductRes = CaseProductRes
  { _case   :: Case.Case,
    _product  :: Product.Products,
    _caseProduct :: CaseProduct,
    _fromLocation :: Maybe Loc.Location,
    _toLocation :: Maybe Loc.Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseProductRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseProductRes where
  toJSON = genericToJSON stripAllLensPrefixOptions


type CaseProductList = [CaseProductRes]