{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.NearestRoad
  ( API,
    handler,
  )
where

import qualified API.Types.UI.NearestRoad
import qualified Control.Lens
import qualified Domain.Action.UI.NearestRoad
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API = (TokenAuth :> "nearestRoad" :> ReqBody ('[JSON]) API.Types.UI.NearestRoad.NearestRoadReq :> Post ('[JSON]) API.Types.UI.NearestRoad.NearestRoadResp)

handler :: Environment.FlowServer API
handler = postNearestRoad

postNearestRoad ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearestRoad.NearestRoadReq ->
    Environment.FlowHandler API.Types.UI.NearestRoad.NearestRoadResp
  )
postNearestRoad a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.NearestRoad.postNearestRoad (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
