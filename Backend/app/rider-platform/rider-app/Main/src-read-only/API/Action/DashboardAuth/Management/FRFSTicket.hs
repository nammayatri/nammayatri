{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.FRFSTicket
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.Dashboard.FRFSTicket
import qualified Domain.Types.Merchant
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

type API = ("fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> GetFRFSTicketFrfsRouteFareList :<|> PutFRFSTicketFrfsRouteFareUpsert :<|> GetFRFSTicketFrfsRouteStations :<|> GetFRFSTicketFrfsGtfs :<|> PostFRFSTicketFrfsStatusUpdate))

type GetFRFSTicketFrfsRoutes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTES"
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRoutes
  )

type GetFRFSTicketFrfsRouteFareList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTE_FARE_LIST"
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteFareList
  )

type PutFRFSTicketFrfsRouteFareUpsert =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/PUT_FRFS_TICKET_FRFS_ROUTE_FARE_UPSERT"
      :> API.Types.RiderPlatform.Management.FRFSTicket.PutFRFSTicketFrfsRouteFareUpsert
  )

type GetFRFSTicketFrfsRouteStations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTE_STATIONS"
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteStations
  )

type GetFRFSTicketFrfsGtfs =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_GTFS"
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsGtfs
  )

type PostFRFSTicketFrfsStatusUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATUS_UPDATE"
      :> API.Types.RiderPlatform.Management.FRFSTicket.PostFRFSTicketFrfsStatusUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSTicketFrfsRoutes merchantId city :<|> getFRFSTicketFrfsRouteFareList merchantId city :<|> putFRFSTicketFrfsRouteFareUpsert merchantId city :<|> getFRFSTicketFrfsRouteStations merchantId city :<|> getFRFSTicketFrfsGtfs merchantId city :<|> postFRFSTicketFrfsStatusUpdate merchantId city

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI])
getFRFSTicketFrfsRoutes a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRoutes a7 a6 a4 a3 a2 a1

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRouteFareList a6 a5 a3 a2 a1

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.putFRFSTicketFrfsRouteFareUpsert a7 a6 a4 a3 a2 a1

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRouteStations a7 a6 a4 a3 a2 a1

getFRFSTicketFrfsGtfs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig) -> Kernel.Prelude.Maybe (Dashboard.Common.PlatformType) -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.FRFSGtfsRes)
getFRFSTicketFrfsGtfs a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsGtfs a6 a5 a3 a2 a1

postFRFSTicketFrfsStatusUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStatusUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStatusUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.postFRFSTicketFrfsStatusUpdate a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
