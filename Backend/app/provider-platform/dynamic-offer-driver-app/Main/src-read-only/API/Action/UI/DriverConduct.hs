{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverConduct
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverConduct
import qualified Control.Lens
import qualified Domain.Action.UI.DriverConduct
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

type API = (TokenAuth :> "driver" :> "conduct" :> "current" :> Get ('[JSON]) API.Types.UI.DriverConduct.DriverConductCurrentRes)

handler :: Environment.FlowServer API
handler = getDriverConductCurrent

getDriverConductCurrent ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverConduct.DriverConductCurrentRes
  )
getDriverConductCurrent a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.DriverConduct.getDriverConductCurrent (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
