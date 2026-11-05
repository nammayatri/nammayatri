{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCabInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCabInternal
import qualified Domain.Action.UI.SharedCabInternal
import qualified Domain.Types.SharedCabSession
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth
import qualified Tools.Notifications

type API =
  ( "sharedCabAllocationFCM" :> Header "token" Kernel.Prelude.Text :> ReqBody '[JSON] Tools.Notifications.SharedCabAllocationEntityData
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "sharedCab"
      :> "driver"
      :> Capture "driverId" Kernel.Prelude.Text
      :> "callNumber"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverCallNumberResp
      :<|> "sharedCab"
      :> "cab"
      :> Capture
           "vehicleNumber"
           Kernel.Prelude.Text
      :> "event"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Domain.Types.SharedCabSession.CabEventReq
      :> Post
           '[JSON]
           Domain.Types.SharedCabSession.CabEventResp
  )

handler :: Environment.FlowServer API
handler = postSharedCabAllocationFCM :<|> getSharedCabDriverCallNumber :<|> postSharedCabCabEvent

postSharedCabAllocationFCM :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Tools.Notifications.SharedCabAllocationEntityData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabAllocationFCM a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabAllocationFCM a2 a1

getSharedCabDriverCallNumber :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabDriverCallNumberResp)
getSharedCabDriverCallNumber a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.getSharedCabDriverCallNumber a2 a1

postSharedCabCabEvent :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SharedCabSession.CabEventReq -> Environment.FlowHandler Domain.Types.SharedCabSession.CabEventResp)
postSharedCabCabEvent a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabCabEvent a3 a2 a1
