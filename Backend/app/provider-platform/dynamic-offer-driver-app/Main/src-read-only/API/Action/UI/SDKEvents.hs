{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SDKEvents where

import qualified API.Types.UI.SDKEvents
import qualified Control.Lens
import qualified Domain.Action.UI.SDKEvents as Domain.Action.UI.SDKEvents
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "sdk" :> "events" :> ReqBody '[JSON] API.Types.UI.SDKEvents.SDKEventsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler = postSdkEvents

postSdkEvents :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.SDKEvents.SDKEventsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSdkEvents a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SDKEvents.postSdkEvents (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
