{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.SeatLayout
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.SeatLayout
import qualified Domain.Action.RiderPlatform.AppManagement.SeatLayout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.SeatLayout
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("seatLayout" :> (UpsertSeatLayout :<|> ListSeatLayout :<|> GetSeatLayout :<|> DeleteSeatLayout))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = upsertSeatLayout merchantId city :<|> listSeatLayout merchantId city :<|> getSeatLayout merchantId city :<|> deleteSeatLayout merchantId city

type UpsertSeatLayout =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SEAT_LAYOUT / 'API.Types.Dashboard.AppManagement.SeatLayout.UPSERT_SEAT_LAYOUT)
      :> API.Types.Dashboard.AppManagement.SeatLayout.UpsertSeatLayout
  )

type ListSeatLayout =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SEAT_LAYOUT / 'API.Types.Dashboard.AppManagement.SeatLayout.LIST_SEAT_LAYOUT)
      :> API.Types.Dashboard.AppManagement.SeatLayout.ListSeatLayout
  )

type GetSeatLayout =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SEAT_LAYOUT / 'API.Types.Dashboard.AppManagement.SeatLayout.GET_SEAT_LAYOUT)
      :> API.Types.Dashboard.AppManagement.SeatLayout.GetSeatLayout
  )

type DeleteSeatLayout =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SEAT_LAYOUT / 'API.Types.Dashboard.AppManagement.SeatLayout.DELETE_SEAT_LAYOUT)
      :> API.Types.Dashboard.AppManagement.SeatLayout.DeleteSeatLayout
  )

upsertSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertSeatLayout merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.SeatLayout.upsertSeatLayout merchantShortId opCity apiTokenInfo req

listSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [Domain.Types.SeatLayout.SeatLayout])
listSeatLayout merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.SeatLayout.listSeatLayout merchantShortId opCity apiTokenInfo limit offset

getSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler API.Types.Dashboard.AppManagement.SeatLayout.SeatLayoutDetailResp)
getSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.SeatLayout.getSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId

deleteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.SeatLayout.deleteSeatLayout merchantShortId opCity apiTokenInfo seatLayoutId
