{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.SeatLayout
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.SeatLayout
import qualified Domain.Action.Dashboard.AppManagement.SeatLayout
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.SeatLayout
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("seatLayout" :> (UpsertSeatLayout :<|> ListSeatLayout :<|> GetSeatLayout :<|> DeleteSeatLayout))

type UpsertSeatLayout = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/SEAT_LAYOUT/UPSERT_SEAT_LAYOUT" :> API.Types.Dashboard.AppManagement.SeatLayout.UpsertSeatLayout)

type ListSeatLayout = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/SEAT_LAYOUT/LIST_SEAT_LAYOUT" :> API.Types.Dashboard.AppManagement.SeatLayout.ListSeatLayout)

type GetSeatLayout = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/SEAT_LAYOUT/GET_SEAT_LAYOUT" :> API.Types.Dashboard.AppManagement.SeatLayout.GetSeatLayout)

type DeleteSeatLayout = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/SEAT_LAYOUT/DELETE_SEAT_LAYOUT" :> API.Types.Dashboard.AppManagement.SeatLayout.DeleteSeatLayout)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = upsertSeatLayout merchantId city :<|> listSeatLayout merchantId city :<|> getSeatLayout merchantId city :<|> deleteSeatLayout merchantId city

upsertSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertSeatLayout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.upsertSeatLayout a4 a3 a1

listSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler [Domain.Types.SeatLayout.SeatLayout])
listSeatLayout a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.listSeatLayout a5 a4 a2 a1

getSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutDetailResp)
getSeatLayout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.getSeatLayout a4 a3 a1

deleteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteSeatLayout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.deleteSeatLayout a4 a3 a1
