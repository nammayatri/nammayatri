{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Frontend
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Frontend
import qualified Domain.Action.Dashboard.RideBooking.Frontend
import qualified "this" Domain.Action.UI.Frontend
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("frontend" :> (GetFrontendFlowStatus :<|> PostFrontendNotifyEvent))

type GetFrontendFlowStatus = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/FRONTEND/GET_FRONTEND_FLOW_STATUS" :> API.Types.Dashboard.RideBooking.Frontend.GetFrontendFlowStatus)

type PostFrontendNotifyEvent = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/FRONTEND/POST_FRONTEND_NOTIFY_EVENT" :> API.Types.Dashboard.RideBooking.Frontend.PostFrontendNotifyEvent)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFrontendFlowStatus merchantId city :<|> postFrontendNotifyEvent merchantId city

getFrontendFlowStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler Domain.Action.UI.Frontend.GetPersonFlowStatusRes)
getFrontendFlowStatus a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Frontend.getFrontendFlowStatus a6 a5 a3 a2 a1

postFrontendNotifyEvent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> Environment.FlowHandler Domain.Action.UI.Frontend.NotifyEventResp)
postFrontendNotifyEvent a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Frontend.postFrontendNotifyEvent a5 a4 a2 a1
