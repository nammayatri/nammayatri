{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.RiderPlatform.Customer
  ( module Dashboard.RiderPlatform.Customer,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import qualified Dashboard.Common as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant

data CustomerEndpoint
  = DeleteCustomerEndpoint
  deriving (Show, Read)

derivePersistField "CustomerEndpoint"

---------------------------------------------------------
-- customer delete --------------------------------------

type CustomerDeleteAPI =
  Capture "customerId" (Id DP.Customer)
    :> "delete"
    :> Post '[JSON] APISuccess
