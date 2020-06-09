module Types.API.Products where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Types.Storage.Vehicle
import Data.Default
import Data.Swagger
import Data.Time
import EulerHS.Prelude

data ProdReq = ProdReq
  { _status :: Maybe Product.ProductsStatus,
    _assignedTo :: Maybe Text,
    _vehicleId :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdInfoRes = Product.Products

data ProdRes = ProdRes
  { _product :: Product.Products,
    _fromLocation :: Location,
    _toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdListRes = [ProdRes]
