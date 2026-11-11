{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverAccountHealth
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverAccountHealth
import qualified Control.Lens
import qualified Domain.Action.UI.DriverAccountHealth
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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

type API = (TokenAuth :> "driver" :> "accountHealth" :> Get ('[JSON]) API.Types.UI.DriverAccountHealth.AccountHealthRes)

handler :: Environment.FlowServer API
handler = getDriverAccountHealth

getDriverAccountHealth ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverAccountHealth.AccountHealthRes
  )
getDriverAccountHealth a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.DriverAccountHealth.getDriverAccountHealth (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
