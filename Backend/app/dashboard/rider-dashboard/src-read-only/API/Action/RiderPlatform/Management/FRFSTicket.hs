{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.FRFSTicket
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.FRFSTicket
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> GetFRFSTicketFrfsRouteFareList :<|> PutFRFSTicketFrfsRouteFareUpsert :<|> GetFRFSTicketFrfsRouteStations))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSTicketFrfsRoutes merchantId city :<|> getFRFSTicketFrfsRouteFareList merchantId city :<|> putFRFSTicketFrfsRouteFareUpsert merchantId city :<|> getFRFSTicketFrfsRouteStations merchantId city

type GetFRFSTicketFrfsRoutes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.FRFS_TICKET / 'API.Types.RiderPlatform.Management.FRFSTicket.GET_FRFS_TICKET_FRFS_ROUTES)
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRoutes
  )

type GetFRFSTicketFrfsRouteFareList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.FRFS_TICKET / 'API.Types.RiderPlatform.Management.FRFSTicket.GET_FRFS_TICKET_FRFS_ROUTE_FARE_LIST)
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteFareList
  )

type PutFRFSTicketFrfsRouteFareUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.FRFS_TICKET / 'API.Types.RiderPlatform.Management.FRFSTicket.PUT_FRFS_TICKET_FRFS_ROUTE_FARE_UPSERT)
      :> API.Types.RiderPlatform.Management.FRFSTicket.PutFRFSTicketFrfsRouteFareUpsert
  )

type GetFRFSTicketFrfsRouteStations =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.FRFS_TICKET / 'API.Types.RiderPlatform.Management.FRFSTicket.GET_FRFS_TICKET_FRFS_ROUTE_STATIONS)
      :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteStations
  )

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI])
getFRFSTicketFrfsRoutes merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRoutes merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList merchantShortId opCity apiTokenInfo routeCode integratedBppConfigId vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRouteFareList merchantShortId opCity apiTokenInfo routeCode integratedBppConfigId vehicleType

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> Kernel.Types.Id.Id Dashboard.Common.IntegratedBPPConfig -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity apiTokenInfo routeCode integratedBppConfigId vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity apiTokenInfo routeCode integratedBppConfigId vehicleType req

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRouteStations merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType
