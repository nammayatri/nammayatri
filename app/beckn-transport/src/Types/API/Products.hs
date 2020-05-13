module Types.API.Products where

import           Data.Default
import           Data.Swagger
import           Data.Time
import           Beckn.Types.Common
import           Beckn.Types.App
import qualified Beckn.Types.Storage.Products   as Product
import           EulerHS.Prelude
import           Types.Storage.Driver
import           Types.Storage.Vehicle

data ProdReq = ProdReq
  { _status   :: Product.ProductsStatus,
    _id     :: Text,
    _driverInfo  :: Driver,
    _vehicleInfo :: Vehicle,
    _assignedTo :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdInfoRes = Text

type RideList = [Product.Products]
