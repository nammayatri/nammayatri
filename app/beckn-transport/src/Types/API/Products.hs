module Types.API.Products where

import Beckn.Types.App
import Beckn.Types.Core.Amount
import qualified Beckn.Types.Storage.Products as Product
import Data.Default
import Data.Swagger
import Data.Time
import EulerHS.Prelude

data CreateProdReq = CreateProdReq
  { _name :: Text,
    _description :: Maybe Text,
    _info :: Maybe Text,
    _rating :: Maybe Text,
    _review :: Maybe Text,
    _udf1 :: Maybe Text,
    _udf2 :: Maybe Text,
    _udf3 :: Maybe Text,
    _udf4 :: Maybe Text,
    _udf5 :: Maybe Text,
    _price :: Maybe Amount
  }
  deriving (Generic, ToSchema)

instance FromJSON CreateProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CreateProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdRes = Product.Products
