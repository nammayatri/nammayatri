module Types.API.Case where

import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import Beckn.Types.Storage.Products
import Data.Default
import Data.Swagger
import EulerHS.Prelude

data StatusRes = StatusRes
  { _case :: Case,
    _product :: [Products],
    _fromLocation :: Location,
    _toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON StatusRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON StatusRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

data UpdateCaseReq = UpdateCaseReq
  { _quote :: Maybe Double,
    _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ListRes = ListRes
  { _cases :: [Case]
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ListRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
