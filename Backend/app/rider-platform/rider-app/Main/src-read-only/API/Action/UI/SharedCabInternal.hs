{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCabInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCabInternal
import qualified Domain.Action.UI.SharedCabInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "sharedCab" :> "routes" :> MandatoryQueryParam "integratedBppConfigId" Kernel.Prelude.Text :> MandatoryQueryParam "lat" Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> Header "token" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           [API.Types.UI.SharedCabInternal.SharedCabRouteResp]
      :<|> "sharedCab"
      :> "route"
      :> "select"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSelectReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
      :<|> "sharedCab"
      :> "session"
      :> MandatoryQueryParam
           "driverId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
      :<|> "sharedCab"
      :> "seats"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSeatsReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
      :<|> "sharedCab"
      :> "route"
      :> "change"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabChangeRouteReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
      :<|> "sharedCab"
      :> "route"
      :> "end"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabEndReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
      :<|> "sharedCab"
      :> "resume"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabDriverReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCabInternal.SharedCabSessionResp
  )

handler :: Environment.FlowServer API
handler = getSharedCabRoutes :<|> postSharedCabRouteSelect :<|> getSharedCabSession :<|> postSharedCabSeats :<|> postSharedCabRouteChange :<|> postSharedCabRouteEnd :<|> postSharedCabResume

getSharedCabRoutes :: (Kernel.Prelude.Text -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [API.Types.UI.SharedCabInternal.SharedCabRouteResp])
getSharedCabRoutes a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.getSharedCabRoutes a4 a3 a2 a1

postSharedCabRouteSelect :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.SharedCabInternal.SharedCabSelectReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
postSharedCabRouteSelect a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabRouteSelect a2 a1

getSharedCabSession :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
getSharedCabSession a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.getSharedCabSession a3 a2 a1

postSharedCabSeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.SharedCabInternal.SharedCabSeatsReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
postSharedCabSeats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabSeats a2 a1

postSharedCabRouteChange :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.SharedCabInternal.SharedCabChangeRouteReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
postSharedCabRouteChange a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabRouteChange a2 a1

postSharedCabRouteEnd :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.SharedCabInternal.SharedCabEndReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
postSharedCabRouteEnd a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabRouteEnd a2 a1

postSharedCabResume :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.SharedCabInternal.SharedCabDriverReq -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabSessionResp)
postSharedCabResume a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCabInternal.postSharedCabResume a2 a1
