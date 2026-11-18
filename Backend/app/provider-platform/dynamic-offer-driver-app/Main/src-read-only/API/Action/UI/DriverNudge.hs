{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverNudge
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverNudge
import qualified Control.Lens
import qualified Domain.Action.UI.DriverNudge
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "demandHotspot" :> "response" :> ReqBody '[JSON] API.Types.UI.DriverNudge.DemandNudgeResponseReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "internal"
      :> "nudge"
      :> Capture "nudgeId" Kernel.Prelude.Text
      :> "start"
      :> Header
           "x-internal-api-key"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverNudge.NudgeStartReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "internal"
      :> "nudge"
      :> Capture
           "nudgeId"
           Kernel.Prelude.Text
      :> "end"
      :> Header
           "x-internal-api-key"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverNudge.NudgeEndReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "internal"
      :> "nudge"
      :> Capture
           "nudgeId"
           Kernel.Prelude.Text
      :> "bulkLocUpdate"
      :> Header
           "api-key"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverNudge.NudgeBulkLocUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverDemandHotspotResponse :<|> postInternalNudgeStart :<|> postInternalNudgeEnd :<|> postInternalNudgeBulkLocUpdate

postDriverDemandHotspotResponse ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverNudge.DemandNudgeResponseReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverDemandHotspotResponse a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.DriverNudge.postDriverDemandHotspotResponse (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postInternalNudgeStart :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.DriverNudge.NudgeStartReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInternalNudgeStart a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.DriverNudge.postInternalNudgeStart a3 a2 a1

postInternalNudgeEnd :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.DriverNudge.NudgeEndReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInternalNudgeEnd a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.DriverNudge.postInternalNudgeEnd a3 a2 a1

postInternalNudgeBulkLocUpdate :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.DriverNudge.NudgeBulkLocUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInternalNudgeBulkLocUpdate a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.DriverNudge.postInternalNudgeBulkLocUpdate a3 a2 a1
