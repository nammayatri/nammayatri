{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Frontend
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Frontend
import qualified Domain.Action.RiderPlatform.RideBooking.Frontend
import qualified "rider-app" Domain.Action.UI.Frontend
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("frontend" :> (GetFrontendFlowStatus :<|> PostFrontendNotifyEvent))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFrontendFlowStatus merchantId city :<|> postFrontendNotifyEvent merchantId city

type GetFrontendFlowStatus =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.FRONTEND / 'API.Types.Dashboard.RideBooking.Frontend.GET_FRONTEND_FLOW_STATUS)
      :> API.Types.Dashboard.RideBooking.Frontend.GetFrontendFlowStatus
  )

type PostFrontendNotifyEvent =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.FRONTEND / 'API.Types.Dashboard.RideBooking.Frontend.POST_FRONTEND_NOTIFY_EVENT)
      :> API.Types.Dashboard.RideBooking.Frontend.PostFrontendNotifyEvent
  )

getFrontendFlowStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler Domain.Action.UI.Frontend.GetPersonFlowStatusRes)
getFrontendFlowStatus merchantShortId opCity apiTokenInfo customerId isPolling checkForActiveBooking = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Frontend.getFrontendFlowStatus merchantShortId opCity apiTokenInfo customerId isPolling checkForActiveBooking

postFrontendNotifyEvent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> Environment.FlowHandler Domain.Action.UI.Frontend.NotifyEventResp)
postFrontendNotifyEvent merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Frontend.postFrontendNotifyEvent merchantShortId opCity apiTokenInfo customerId req
