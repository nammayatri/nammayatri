module Types.API.CaseProduct where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.CaseProduct
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Products as Product
import Data.Default
import Data.Swagger
import Data.Time
import EulerHS.Prelude

data CaseProdReq = CaseProdReq
  { _status :: [CaseProduct.CaseProductStatus],
    _limit :: Integer,
    _offset :: Integer
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data CaseProductRes = CaseProductRes
  { _case :: Case.Case,
    _product :: Product.Products,
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
