module Types.API.Products where

import Beckn.Types.Amount
import Beckn.Utils.JSON
import Data.Swagger hiding (description, info, name)
import EulerHS.Prelude
import qualified Types.Storage.Products as Product

data CreateProdReq = CreateProdReq
  { name :: Text,
    description :: Maybe Text,
    info :: Maybe Text,
    rating :: Maybe Text,
    review :: Maybe Text,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    price :: Maybe Amount
  }
  deriving (Generic, ToSchema)

instance FromJSON CreateProdReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CreateProdReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type ProdRes = Product.Products
