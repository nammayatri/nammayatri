{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSInternal
import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSInternal
import qualified Domain.Types.FavouriteRoute
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "frfs" :> "trip" :> Capture "tripId" Kernel.Prelude.Text :> "route" :> Capture "routeId" Kernel.Prelude.Text :> "manifest" :> Header "token" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp
      :<|> "frfs"
      :> "trip"
      :> Capture
           "tripId"
           Kernel.Prelude.Text
      :> "notifyTripStarted"
      :> QueryParam
           "source"
           Domain.Types.FavouriteRoute.NotifyTripStartedSource
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> "frfs"
      :> "trip"
      :> Capture
           "tripId"
           Kernel.Prelude.Text
      :> "stop"
      :> Capture
           "stopCode"
           Kernel.Prelude.Text
      :> "notifyApproaching"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSInternal.NotifyBusApproachingReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> "frfs"
      :> "trip"
      :> Capture
           "tripId"
           Kernel.Prelude.Text
      :> "notifyTripStarted"
      :> "v2"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSInternal.NotifyTripStartedReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getFrfsTripRouteManifest :<|> postFrfsTripNotifyTripStarted :<|> postFrfsTripStopNotifyApproaching :<|> postFrfsTripNotifyTripStartedV2

getFrfsTripRouteManifest :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp)
getFrfsTripRouteManifest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSInternal.getFrfsTripRouteManifest a3 a2 a1

postFrfsTripNotifyTripStarted :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Domain.Types.FavouriteRoute.NotifyTripStartedSource) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFrfsTripNotifyTripStarted a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSInternal.postFrfsTripNotifyTripStarted a3 a2 a1

postFrfsTripStopNotifyApproaching :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSInternal.NotifyBusApproachingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFrfsTripStopNotifyApproaching a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSInternal.postFrfsTripStopNotifyApproaching a4 a3 a2 a1

postFrfsTripNotifyTripStartedV2 :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSInternal.NotifyTripStartedReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFrfsTripNotifyTripStartedV2 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSInternal.postFrfsTripNotifyTripStartedV2 a3 a2 a1
