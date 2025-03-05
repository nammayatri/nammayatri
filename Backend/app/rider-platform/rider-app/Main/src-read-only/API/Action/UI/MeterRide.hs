{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MeterRide
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MeterRide
import qualified Domain.Action.UI.MeterRide as Domain.Action.UI.MeterRide
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( Capture "bppRideId" Kernel.Prelude.Text :> "addDestination" :> Header "token" Kernel.Prelude.Text :> ReqBody ('[JSON]) API.Types.UI.MeterRide.MeterRideAddDestinationReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postAddDestination

postAddDestination :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.MeterRide.MeterRideAddDestinationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAddDestination a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MeterRide.postAddDestination a3 a2 a1
