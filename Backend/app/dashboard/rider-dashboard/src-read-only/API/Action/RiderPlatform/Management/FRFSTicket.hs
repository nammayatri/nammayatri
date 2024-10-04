{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.FRFSTicket
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.FRFSTicket
import qualified "lib-dashboard" Domain.Types.Merchant
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

type API = ("fRFSTicket" :> (GetFRFSTicketFrfsRoutes :<|> PostFRFSTicketFrfsRouteAdd :<|> PostFRFSTicketFrfsRouteDelete :<|> GetFRFSTicketFrfsRouteFareList :<|> PutFRFSTicketFrfsRouteFareUpsert :<|> GetFRFSTicketFrfsRouteStations :<|> PostFRFSTicketFrfsStationAdd :<|> PostFRFSTicketFrfsStationDelete))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSTicketFrfsRoutes merchantId city :<|> postFRFSTicketFrfsRouteAdd merchantId city :<|> postFRFSTicketFrfsRouteDelete merchantId city :<|> getFRFSTicketFrfsRouteFareList merchantId city :<|> putFRFSTicketFrfsRouteFareUpsert merchantId city :<|> getFRFSTicketFrfsRouteStations merchantId city :<|> postFRFSTicketFrfsStationAdd merchantId city :<|> postFRFSTicketFrfsStationDelete merchantId city

type GetFRFSTicketFrfsRoutes = (ApiAuth 'APP_BACKEND 'FRFS 'LIST_FRFS_ROUTES :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRoutes)

type PostFRFSTicketFrfsRouteAdd = (ApiAuth 'APP_BACKEND 'FRFS 'ADD_FRFS_ROUTE :> API.Types.RiderPlatform.Management.FRFSTicket.PostFRFSTicketFrfsRouteAdd)

type PostFRFSTicketFrfsRouteDelete = (ApiAuth 'APP_BACKEND 'FRFS 'DELETE_FRFS_ROUTE :> API.Types.RiderPlatform.Management.FRFSTicket.PostFRFSTicketFrfsRouteDelete)

type GetFRFSTicketFrfsRouteFareList = (ApiAuth 'APP_BACKEND 'FRFS 'LIST_FRFS_ROUTE_FARE :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteFareList)

type PutFRFSTicketFrfsRouteFareUpsert = (ApiAuth 'APP_BACKEND 'FRFS 'UPSERT_FRFS_ROUTE_FARE :> API.Types.RiderPlatform.Management.FRFSTicket.PutFRFSTicketFrfsRouteFareUpsert)

type GetFRFSTicketFrfsRouteStations = (ApiAuth 'APP_BACKEND 'FRFS 'LIST_FRFS_STATION :> API.Types.RiderPlatform.Management.FRFSTicket.GetFRFSTicketFrfsRouteStations)

type PostFRFSTicketFrfsStationAdd = (ApiAuth 'APP_BACKEND 'FRFS 'ADD_FRFS_STATION :> API.Types.RiderPlatform.Management.FRFSTicket.PostFRFSTicketFrfsStationAdd)

type PostFRFSTicketFrfsStationDelete = (ApiAuth 'APP_BACKEND 'FRFS 'DELETE_FRFS_STATION :> API.Types.RiderPlatform.Management.FRFSTicket.PostFRFSTicketFrfsStationDelete)

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteAPI])
getFRFSTicketFrfsRoutes merchantShortId opCity apiTokenInfo limit offset vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRoutes merchantShortId opCity apiTokenInfo limit offset vehicleType

postFRFSTicketFrfsRouteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteAdd merchantShortId opCity apiTokenInfo code vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.postFRFSTicketFrfsRouteAdd merchantShortId opCity apiTokenInfo code vehicleType req

postFRFSTicketFrfsRouteDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteDelete merchantShortId opCity apiTokenInfo code vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.postFRFSTicketFrfsRouteDelete merchantShortId opCity apiTokenInfo code vehicleType

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI])
getFRFSTicketFrfsRouteFareList merchantShortId opCity apiTokenInfo routeCode vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRouteFareList merchantShortId opCity apiTokenInfo routeCode vehicleType

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity apiTokenInfo routeCode vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity apiTokenInfo routeCode vehicleType req

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.getFRFSTicketFrfsRouteStations merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType

postFRFSTicketFrfsStationAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationAdd merchantShortId opCity apiTokenInfo code vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.postFRFSTicketFrfsStationAdd merchantShortId opCity apiTokenInfo code vehicleType req

postFRFSTicketFrfsStationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationDelete merchantShortId opCity apiTokenInfo code vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.FRFSTicket.postFRFSTicketFrfsStationDelete merchantShortId opCity apiTokenInfo code vehicleType
