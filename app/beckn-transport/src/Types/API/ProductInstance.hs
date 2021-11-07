module Types.API.ProductInstance where

import Beckn.Utils.JSON
import Data.Swagger
import EulerHS.Prelude hiding (product)
import Types.API.Person (PersonEntityRes)
import qualified Types.Storage.Case as Case
import Types.Storage.ProductInstance
import qualified Types.Storage.Products as Product
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.Vehicle as SVeh

data ProductInstanceRes = ProductInstanceRes
  { _case :: Case.Case,
    product :: Product.Products,
    productInstance :: ProductInstance,
    fromLocation :: Maybe Loc.SearchReqLocation,
    toLocation :: Maybe Loc.SearchReqLocation,
    driver :: Maybe PersonEntityRes,
    vehicle :: Maybe SVeh.Vehicle
  }
  deriving (Generic, Show)

instance FromJSON ProductInstanceRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProductInstanceRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type ProductInstanceList = [ProductInstanceRes]

data RideRes = RideRes
  { product :: ProductInstance,
    fromLocation :: Loc.SearchReqLocation,
    toLocation :: Loc.SearchReqLocation
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type RideListRes = [RideRes]
