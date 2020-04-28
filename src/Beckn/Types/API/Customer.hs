module Beckn.Types.API.Customer where

import EulerHS.Prelude
import Beckn.Types.Storage.Customer
import Servant.Swagger
import Data.Swagger

data GetCustomerRes =
  GetCustomerRes
   { customer :: Customer
   } deriving (Generic, ToJSON, ToSchema)
