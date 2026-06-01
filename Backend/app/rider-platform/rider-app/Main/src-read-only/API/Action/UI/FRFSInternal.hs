{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "frfs" :> "trip" :> Capture "tripId" Kernel.Prelude.Text :> "route" :> Capture "routeId" Kernel.Prelude.Text :> "manifest" :> Header "token" Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp
  )

handler :: Environment.FlowServer API
handler = getFrfsTripRouteManifest

getFrfsTripRouteManifest :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp)
getFrfsTripRouteManifest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSInternal.getFrfsTripRouteManifest a3 a2 a1
