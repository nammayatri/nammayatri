{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.UnserviceableCityLead
  ( API,
    handler,
  )
where

import qualified API.Types.UI.UnserviceableCityLead
import qualified Control.Lens
import qualified Domain.Action.UI.UnserviceableCityLead
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( "unserviceableCity" :> "lead" :> ReqBody '[JSON] API.Types.UI.UnserviceableCityLead.UnserviceableCityLeadReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postUnserviceableCityLead

postUnserviceableCityLead :: (API.Types.UI.UnserviceableCityLead.UnserviceableCityLeadReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUnserviceableCityLead a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.UnserviceableCityLead.postUnserviceableCityLead a1
