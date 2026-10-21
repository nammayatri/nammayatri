{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SearchRoute
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SearchRoute
import qualified Control.Lens
import qualified Domain.Action.UI.SearchRoute
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchTry
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "search" :> Capture "searchTryId" (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry) :> "route" :> ReqBody ('[JSON]) Kernel.External.Maps.GetRoutesReq
      :> Post
           ('[JSON])
           API.Types.UI.SearchRoute.SearchRouteResp
  )

handler :: Environment.FlowServer API
handler = postSearchRoute

postSearchRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry ->
    Kernel.External.Maps.GetRoutesReq ->
    Environment.FlowHandler API.Types.UI.SearchRoute.SearchRouteResp
  )
postSearchRoute a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.SearchRoute.postSearchRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
