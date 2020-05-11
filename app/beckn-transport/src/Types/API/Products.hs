module Types.API.Products where

import           Data.Default
import           Data.Swagger
import           Data.Time
import           Beckn.Types.Common
import           Beckn.Types.App
import qualified Beckn.Types.Storage.Products   as Product
import           EulerHS.Prelude

data ProdReq = ProdReq
  { _status   :: Product.ProductsStatus,
    _id     :: Text,
    _driverId  :: Text,
    _vehicleId :: Text,
    _orgId :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ProdInfoRes = Text
