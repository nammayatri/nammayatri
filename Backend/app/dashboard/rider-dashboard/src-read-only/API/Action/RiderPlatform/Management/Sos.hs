{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Sos
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Sos
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.Sos
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("sos" :> (GetSosTracking :<|> GetSosDetails :<|> PostSosCallExternalSOS :<|> PostSosErssStatusUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosTracking merchantId city :<|> getSosDetails merchantId city :<|> postSosCallExternalSOS merchantId city :<|> postSosErssStatusUpdate merchantId city

type GetSosTracking = API.Types.RiderPlatform.Management.Sos.GetSosTracking

type GetSosDetails = API.Types.RiderPlatform.Management.Sos.GetSosDetails

type PostSosCallExternalSOS =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.SOS / 'API.Types.RiderPlatform.Management.Sos.POST_SOS_CALL_EXTERNAL_SOS)
      :> API.Types.RiderPlatform.Management.Sos.PostSosCallExternalSOS
  )

type PostSosErssStatusUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.SOS / 'API.Types.RiderPlatform.Management.Sos.POST_SOS_ERSS_STATUS_UPDATE)
      :> API.Types.RiderPlatform.Management.Sos.PostSosErssStatusUpdate
  )

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking merchantShortId opCity sosId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Sos.getSosTracking merchantShortId opCity sosId

getSosDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes)
getSosDetails merchantShortId opCity sosId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Sos.getSosDetails merchantShortId opCity sosId

postSosCallExternalSOS :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Sos -> API.Types.RiderPlatform.Management.Sos.CallExternalSOSReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSosCallExternalSOS merchantShortId opCity apiTokenInfo sosId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Sos.postSosCallExternalSOS merchantShortId opCity apiTokenInfo sosId req

postSosErssStatusUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateRes)
postSosErssStatusUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Sos.postSosErssStatusUpdate merchantShortId opCity apiTokenInfo req
