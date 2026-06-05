{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.SeatLayout
  ( API.Types.Dashboard.AppManagement.SeatLayout.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.SeatLayout.API)
handler merchantId city = upsertSeatLayout merchantId city :<|> listSeatLayout merchantId city :<|> getSeatLayout merchantId city :<|> deleteSeatLayout merchantId city

upsertSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertSeatLayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.upsertSeatLayout a3 a2 a1

listSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutListItem])
listSeatLayout a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.listSeatLayout a4 a3 a2 a1

getSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutDetailResp)
getSeatLayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.getSeatLayout a3 a2 a1

deleteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteSeatLayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SeatLayout.deleteSeatLayout a3 a2 a1
