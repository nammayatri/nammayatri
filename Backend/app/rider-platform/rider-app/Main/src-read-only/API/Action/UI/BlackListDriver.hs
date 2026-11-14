{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.BlackListDriver
  ( API,
    handler,
  )
where

import qualified API.Types.UI.BlackListDriver
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.BlackListDriver
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
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
  ( TokenAuth :> "driver" :> "blackList" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> ReqBody ('[JSON]) API.Types.UI.BlackListDriver.BlackListDriverReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "blacklist"
      :> Get
           ('[JSON])
           [API.Types.UI.BlackListDriver.BlackListedDriverResp]
      :<|> TokenAuth
      :> "driver"
      :> "blacklist"
      :> Capture
           "driverId"
           Data.Text.Text
      :> "unblock"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverBlackList :<|> getDriverBlacklist :<|> postDriverBlacklistUnblock

postDriverBlackList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.BlackListDriver.BlackListDriverReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverBlackList a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.BlackListDriver.postDriverBlackList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getDriverBlacklist ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.BlackListDriver.BlackListedDriverResp]
  )
getDriverBlacklist a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.BlackListDriver.getDriverBlacklist (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postDriverBlacklistUnblock ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverBlacklistUnblock a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.BlackListDriver.postDriverBlacklistUnblock (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
