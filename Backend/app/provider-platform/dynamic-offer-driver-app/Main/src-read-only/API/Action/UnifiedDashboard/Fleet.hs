{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UnifiedDashboard.Fleet
  ( API,
    handler,
  )
where

import qualified API.Action.UnifiedDashboard.Fleet.HealthCheck
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.UnifiedDashboard.Fleet.HealthCheck.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.UnifiedDashboard.Fleet.HealthCheck.handler merchantId city
