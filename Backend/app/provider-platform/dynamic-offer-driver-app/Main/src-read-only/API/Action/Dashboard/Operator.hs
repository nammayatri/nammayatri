{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.Operator.Driver
import qualified API.Action.Dashboard.Operator.FleetManagement
import qualified API.Action.Dashboard.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.Operator.Driver.API :<|> API.Action.Dashboard.Operator.FleetManagement.API :<|> API.Action.Dashboard.Operator.Registration.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.Operator.Driver.handler merchantId city :<|> API.Action.Dashboard.Operator.FleetManagement.handler merchantId city :<|> API.Action.Dashboard.Operator.Registration.handler merchantId city
