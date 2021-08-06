module Types.API.Products where

import Beckn.Types.Amount
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
  deriving (Generic, FromJSON, ToJSON, ToSchema)

type ProdRes = Product.Products
