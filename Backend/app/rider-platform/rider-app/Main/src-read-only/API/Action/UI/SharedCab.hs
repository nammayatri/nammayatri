{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCab
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCab
import qualified Control.Lens
import qualified Domain.Action.UI.SharedCab
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

type API =
  ( TokenAuth :> "sharedCab" :> "routes" :> Get ('[JSON]) API.Types.UI.SharedCab.SharedCabRouteListResp :<|> TokenAuth :> "sharedCab" :> "routes"
      :> Capture
           "routeCode"
           Kernel.Prelude.Text
      :> Get ('[JSON]) API.Types.UI.SharedCab.SharedCabRouteDetailResp
  )

handler :: Environment.FlowServer API
handler = getSharedCabRoutes :<|> getSharedCabRoute

getSharedCabRoutes :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.SharedCab.SharedCabRouteListResp)
getSharedCabRoutes a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCab.getSharedCabRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getSharedCabRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.SharedCab.SharedCabRouteDetailResp
  )
getSharedCabRoute a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCab.getSharedCabRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
