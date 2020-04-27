module Beckn.Types.API.Customer where

import EulerHS.Prelude
import Beckn.Types.Storage.Customer

data GetCustomerRes =
  GetCustomerRes
   { customer :: Customer
   } deriving (Generic, ToJSON)
