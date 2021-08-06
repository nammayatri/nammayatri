module Types.API.ProductInstance where

import Beckn.Utils.JSON
import Data.Swagger
import EulerHS.Prelude hiding (product)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Location as Loc
import Types.Storage.ProductInstance
import qualified Types.Storage.Products as Product

data ProductInstanceRes = ProductInstanceRes
  { _case :: Case.Case,
    product :: Product.Products,
    productInstance :: ProductInstance,
    fromLocation :: Maybe Loc.Location,
    toLocation :: Maybe Loc.Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProductInstanceRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProductInstanceRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type ProductInstanceList = [ProductInstanceRes]

data RideRes = RideRes
  { product :: ProductInstance,
    fromLocation :: Loc.Location,
    toLocation :: Loc.Location
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type RideListRes = [RideRes]
