{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Frontend
  ( API.Types.Dashboard.RideBooking.Frontend.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Frontend.API)
handler merchantId city = getFrontendFlowStatus merchantId city :<|> postFrontendNotifyEvent merchantId city

getFrontendFlowStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler Domain.Action.UI.Frontend.GetPersonFlowStatusRes)
getFrontendFlowStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Frontend.getFrontendFlowStatus a5 a4 a3 a2 a1

postFrontendNotifyEvent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> Environment.FlowHandler Domain.Action.UI.Frontend.NotifyEventResp)
postFrontendNotifyEvent a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Frontend.postFrontendNotifyEvent a4 a3 a2 a1
