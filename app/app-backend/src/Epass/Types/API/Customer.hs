module Epass.Types.API.Customer where

import Data.Aeson
import Data.Swagger
import Epass.Types.Storage.Customer
import Epass.Types.Storage.CustomerDetail (IdentifierType (..))
import EulerHS.Prelude
import Servant.Swagger

data AdditionalInfo = AdditionalInfo
  { _uniqueIdentifier :: Text,
    _identifierType :: IdentifierType,
    _value :: Json,
    _verified :: Bool,
    _primaryIdentifier :: Bool
  }
  deriving (Generic, ToSchema)

instance ToJSON AdditionalInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype Json
  = Json Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToSchema Json where
  declareNamedSchema _ =
    return $ NamedSchema (Just "Json") (sketchSchema Null)

data GetCustomerRes = GetCustomerRes
  { _customer :: Customer,
    _additionalInfo :: [AdditionalInfo]
  }
  deriving (Generic, ToSchema)

instance ToJSON GetCustomerRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
