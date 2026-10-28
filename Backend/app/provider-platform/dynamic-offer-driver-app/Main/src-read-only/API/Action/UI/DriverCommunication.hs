{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverCommunication
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverCommunication
import qualified Control.Lens
import qualified Domain.Action.UI.DriverCommunication
import qualified Domain.Types.Communication
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
  ( TokenAuth :> "driver" :> "communication" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.DriverCommunication.DriverCommunicationListRes
      :<|> TokenAuth
      :> "driver"
      :> "communication"
      :> Capture
           "communicationId"
           (Kernel.Types.Id.Id Domain.Types.Communication.Communication)
      :> "read"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getDriverCommunicationList :<|> postDriverCommunicationRead

getDriverCommunicationList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.FlowHandler API.Types.UI.DriverCommunication.DriverCommunicationListRes
  )
getDriverCommunicationList a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.DriverCommunication.getDriverCommunicationList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postDriverCommunicationRead ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverCommunicationRead a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.DriverCommunication.postDriverCommunicationRead (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
