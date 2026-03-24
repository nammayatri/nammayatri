{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PreferredRoute
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PreferredRoute
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.PreferredRoute



type API = (TokenAuth :> "preferredRoutes" :> Get ('[JSON]) [API.Types.UI.PreferredRoute.PreferredRouteResp])
handler :: Environment.FlowServer API
handler = getPreferredRoutes
getPreferredRoutes :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.PreferredRoute.PreferredRouteResp])
getPreferredRoutes a1 = withFlowHandlerAPI $ Domain.Action.UI.PreferredRoute.getPreferredRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)



