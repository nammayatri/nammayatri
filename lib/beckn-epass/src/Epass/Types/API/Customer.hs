module Epass.Types.API.Customer where

import           Epass.Types.Storage.Customer
import           Data.Swagger
import           EulerHS.Prelude
import           Servant.Swagger

data GetCustomerRes =
  GetCustomerRes
   { customer :: Customer
   } deriving (Generic, ToJSON, ToSchema)
