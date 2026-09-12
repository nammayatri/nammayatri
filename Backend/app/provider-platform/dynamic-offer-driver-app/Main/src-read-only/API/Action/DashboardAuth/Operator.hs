{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Operator
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.Operator.Driver
import qualified API.Action.DashboardAuth.Operator.FleetManagement
import qualified API.Action.DashboardAuth.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.Operator.Driver.API :<|> API.Action.DashboardAuth.Operator.FleetManagement.API :<|> API.Action.DashboardAuth.Operator.Registration.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.Operator.Driver.handler merchantId city :<|> API.Action.DashboardAuth.Operator.FleetManagement.handler merchantId city :<|> API.Action.DashboardAuth.Operator.Registration.handler merchantId city
