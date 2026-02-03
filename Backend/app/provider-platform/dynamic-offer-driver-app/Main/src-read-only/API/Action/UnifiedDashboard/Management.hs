{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UnifiedDashboard.Management
  ( API,
    handler,
  )
where

import qualified API.Action.UnifiedDashboard.Management.HealthCheck
import qualified API.Action.UnifiedDashboard.Management.MediaFileDocument
import qualified API.Action.UnifiedDashboard.Management.Person
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.UnifiedDashboard.Management.HealthCheck.API :<|> API.Action.UnifiedDashboard.Management.MediaFileDocument.API :<|> API.Action.UnifiedDashboard.Management.Person.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.UnifiedDashboard.Management.HealthCheck.handler merchantId city :<|> API.Action.UnifiedDashboard.Management.MediaFileDocument.handler merchantId city :<|> API.Action.UnifiedDashboard.Management.Person.handler merchantId city
