module Types.API.ProductInstance where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import Beckn.Types.Storage.ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import Data.Swagger
import EulerHS.Prelude

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

data ProdInstUpdateReq = ProdInstUpdateReq
  { _status :: ProductInstanceStatus,
    _otpCode :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdInstUpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdInstUpdateReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdInstInfo = ProductInstance

data DriverVehicleInfo = DriverVehicleInfo
  { driverInfo :: Text,
    vehicleInfo :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data RideRes = RideRes
  { _product :: ProductInstance,
    _fromLocation :: Loc.Location,
    _toLocation :: Loc.Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON RideRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RideRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type RideListRes = [RideRes]
