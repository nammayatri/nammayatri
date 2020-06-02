module Epass.Types.API.Customer where

import Beckn.Types.Storage.Person
import Data.Aeson
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data GetCustomerRes = GetCustomerRes
  { _customer :: Person
  }
  deriving (Generic, ToSchema)

instance ToJSON GetCustomerRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
