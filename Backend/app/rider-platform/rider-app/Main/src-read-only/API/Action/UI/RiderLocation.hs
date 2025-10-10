{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RiderLocation
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RiderLocation
import qualified Control.Lens
import qualified Domain.Action.UI.RiderLocation
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "identifyNearByBus" :> ReqBody ('[JSON]) API.Types.UI.RiderLocation.RiderLocationRequest :> Post ('[JSON]) API.Types.UI.RiderLocation.RiderLocationResponse)

handler :: Environment.FlowServer API
handler = postIdentifyNearByBus

postIdentifyNearByBus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RiderLocation.RiderLocationRequest ->
    Environment.FlowHandler API.Types.UI.RiderLocation.RiderLocationResponse
  )
postIdentifyNearByBus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderLocation.postIdentifyNearByBus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
