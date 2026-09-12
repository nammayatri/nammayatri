{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Sos
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("sos" :> (GetSosTracking :<|> GetSosDetails :<|> PostSosCallExternalSOS :<|> PostSosErssStatusUpdate))

type GetSosTracking = API.Types.RiderPlatform.Management.Sos.GetSosTracking

type GetSosDetails = API.Types.RiderPlatform.Management.Sos.GetSosDetails

type PostSosCallExternalSOS = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/SOS/POST_SOS_CALL_EXTERNAL_SOS" :> API.Types.RiderPlatform.Management.Sos.PostSosCallExternalSOS)

type PostSosErssStatusUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/SOS/POST_SOS_ERSS_STATUS_UPDATE" :> API.Types.RiderPlatform.Management.Sos.PostSosErssStatusUpdate)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosTracking merchantId city :<|> getSosDetails merchantId city :<|> postSosCallExternalSOS merchantId city :<|> postSosErssStatusUpdate merchantId city

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.getSosTracking a3 a2 a1

getSosDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes)
getSosDetails a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.getSosDetails a3 a2 a1

postSosCallExternalSOS :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Sos -> API.Types.RiderPlatform.Management.Sos.CallExternalSOSReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSosCallExternalSOS a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.postSosCallExternalSOS a5 a4 a2 a1

postSosErssStatusUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateRes)
postSosErssStatusUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Sos.postSosErssStatusUpdate a4 a3 a1
