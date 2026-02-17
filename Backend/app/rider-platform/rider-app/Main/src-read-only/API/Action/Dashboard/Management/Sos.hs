{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Sos
  ( API.Types.RiderPlatform.Management.Sos.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Sos
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Sos.API)
handler merchantId city = getSosTracking merchantId city :<|> postSosCallExternalSOS merchantId city

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.getSosTracking a3 a2 a1

postSosCallExternalSOS :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSosCallExternalSOS a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.postSosCallExternalSOS a3 a2 a1
