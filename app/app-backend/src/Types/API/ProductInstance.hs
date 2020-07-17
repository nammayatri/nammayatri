module Types.API.ProductInstance where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import Beckn.Types.Storage.ProductInstance
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import Data.Swagger
import EulerHS.Prelude

data ProdInstReq = ProdInstReq
  { _status :: [ProductInstance.ProductInstanceStatus],
    _limit :: Integer,
    _offset :: Integer
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdInstReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdInstReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ProductInstanceRes = ProductInstanceRes
  { _case :: Case.Case,
    _product :: Product.Products,
    _productInstance :: ProductInstance,
    _fromLocation :: Maybe Loc.Location,
    _toLocation :: Maybe Loc.Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProductInstanceRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProductInstanceRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProductInstanceList = [ProductInstanceRes]
