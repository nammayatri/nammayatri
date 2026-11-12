{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.OpenMeter
  ( API,
    handler,
  )
where

import qualified API.Types.UI.OpenMeter
import qualified Control.Lens
import qualified Domain.Action.UI.OpenMeter
import qualified Domain.Types.Merchant
import qualified Domain.Types.OpenMeterRide
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

type API =
  ( TokenAuth :> "openMeter" :> "ride" :> "start" :> ReqBody ('[JSON]) API.Types.UI.OpenMeter.OpenMeterRideStartReq
      :> Post
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideRes
      :<|> TokenAuth
      :> "openMeter"
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide)
      :> "heartbeat"
      :> ReqBody
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideHeartbeatReq
      :> Post
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideRes
      :<|> TokenAuth
      :> "openMeter"
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide)
      :> "end"
      :> ReqBody
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideEndReq
      :> Post
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideRes
      :<|> TokenAuth
      :> "openMeter"
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide)
      :> "cancel"
      :> Post
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideRes
      :<|> TokenAuth
      :> "openMeter"
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide)
      :> Get
           ('[JSON])
           API.Types.UI.OpenMeter.OpenMeterRideRes
      :<|> TokenAuth
      :> "openMeter"
      :> "rides"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.OpenMeterRide.OpenMeterRideStatus
      :> Get
           ('[JSON])
           [API.Types.UI.OpenMeter.OpenMeterRideRes]
  )

handler :: Environment.FlowServer API
handler = postOpenMeterRideStart :<|> postOpenMeterRideHeartbeat :<|> postOpenMeterRideEnd :<|> postOpenMeterRideCancel :<|> getOpenMeterRide :<|> getOpenMeterRides

postOpenMeterRideStart ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.OpenMeter.OpenMeterRideStartReq ->
    Environment.FlowHandler API.Types.UI.OpenMeter.OpenMeterRideRes
  )
postOpenMeterRideStart a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.OpenMeter.postOpenMeterRideStart (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postOpenMeterRideHeartbeat ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide ->
    API.Types.UI.OpenMeter.OpenMeterRideHeartbeatReq ->
    Environment.FlowHandler API.Types.UI.OpenMeter.OpenMeterRideRes
  )
postOpenMeterRideHeartbeat a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.OpenMeter.postOpenMeterRideHeartbeat (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postOpenMeterRideEnd ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide ->
    API.Types.UI.OpenMeter.OpenMeterRideEndReq ->
    Environment.FlowHandler API.Types.UI.OpenMeter.OpenMeterRideRes
  )
postOpenMeterRideEnd a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.OpenMeter.postOpenMeterRideEnd (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postOpenMeterRideCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide ->
    Environment.FlowHandler API.Types.UI.OpenMeter.OpenMeterRideRes
  )
postOpenMeterRideCancel a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.OpenMeter.postOpenMeterRideCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getOpenMeterRide ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide ->
    Environment.FlowHandler API.Types.UI.OpenMeter.OpenMeterRideRes
  )
getOpenMeterRide a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.OpenMeter.getOpenMeterRide (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getOpenMeterRides ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Domain.Types.OpenMeterRide.OpenMeterRideStatus) ->
    Environment.FlowHandler [API.Types.UI.OpenMeter.OpenMeterRideRes]
  )
getOpenMeterRides a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a4) $ Domain.Action.UI.OpenMeter.getOpenMeterRides (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
