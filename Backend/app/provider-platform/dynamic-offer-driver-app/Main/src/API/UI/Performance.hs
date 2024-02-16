module API.UI.Performance where

import qualified Domain.Action.UI.Performance as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "driver" :> "performance"
    :> TokenAuth
    :> Get '[JSON] Domain.PerformanceRes

handler :: FlowServer API
handler =
  getDriverPerformance

getDriverPerformance :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Domain.PerformanceRes
getDriverPerformance = withFlowHandlerAPI . Domain.getDriverPerformance
