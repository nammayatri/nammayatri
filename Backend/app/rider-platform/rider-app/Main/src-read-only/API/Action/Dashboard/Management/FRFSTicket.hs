{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.FRFSTicket
  ( API.Types.RiderPlatform.Management.FRFSTicket.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified Data.Text
import qualified Domain.Action.Dashboard.FRFSTicket as Domain.Action.Dashboard.FRFSTicket
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.FRFSTicket.API)
handler merchantId city = getFRFSTicketFrfsRoutes merchantId city :<|> postFRFSTicketFrfsRouteAdd merchantId city :<|> postFRFSTicketFrfsRouteDelete merchantId city :<|> getFRFSTicketFrfsRouteFareList merchantId city :<|> putFRFSTicketFrfsRouteFareUpsert merchantId city :<|> getFRFSTicketFrfsRouteStations merchantId city :<|> postFRFSTicketFrfsStationAdd merchantId city :<|> postFRFSTicketFrfsStationDelete merchantId city

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteAPI])
getFRFSTicketFrfsRoutes a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRoutes a6 a5 a4 a3 a2 a1

postFRFSTicketFrfsRouteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteAdd a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.postFRFSTicketFrfsRouteAdd a5 a4 a3 a2 a1

postFRFSTicketFrfsRouteDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.postFRFSTicketFrfsRouteDelete a4 a3 a2 a1

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRouteFareList a4 a3 a2 a1

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.putFRFSTicketFrfsRouteFareUpsert a5 a4 a3 a2 a1

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.getFRFSTicketFrfsRouteStations a6 a5 a4 a3 a2 a1

postFRFSTicketFrfsStationAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationAdd a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.postFRFSTicketFrfsStationAdd a5 a4 a3 a2 a1

postFRFSTicketFrfsStationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.FRFSTicket.postFRFSTicketFrfsStationDelete a4 a3 a2 a1
