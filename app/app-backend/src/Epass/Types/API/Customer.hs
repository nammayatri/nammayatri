module Epass.Types.API.Customer where

import Data.Aeson
import Data.Swagger
import Beckn.Types.Storage.Person
import EulerHS.Prelude
import Servant.Swagger

data GetCustomerRes = GetCustomerRes
  { _customer :: Person
  }
  deriving (Generic, ToSchema)

instance ToJSON GetCustomerRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
