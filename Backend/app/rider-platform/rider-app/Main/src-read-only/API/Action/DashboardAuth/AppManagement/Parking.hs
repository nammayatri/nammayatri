{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Parking
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Parking
import qualified "this" API.Types.UI.TicketService
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.Parking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = "parking" :> (ListParkingClusters :<|> (GetParkingCityRevenue :<|> (GetParkingCluster :<|> (SetParkingClusterStatus :<|> (GetParkingOccupancy :<|> (GetParkingClusterRevenue :<|> (VerifyParkingTicket :<|> CashBookParking)))))))

type ListParkingClusters = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/LIST_PARKING_CLUSTERS" :> API.Types.Dashboard.AppManagement.Parking.ListParkingClusters

type GetParkingCityRevenue = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CITY_REVENUE" :> API.Types.Dashboard.AppManagement.Parking.GetParkingCityRevenue

type GetParkingCluster = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER" :> API.Types.Dashboard.AppManagement.Parking.GetParkingCluster

type SetParkingClusterStatus =
  DashboardUserAuth
    'APP_BACKEND_MANAGEMENT
    "RIDER_APP_MANAGEMENT/PARKING/SET_PARKING_CLUSTER_STATUS"
    :> API.Types.Dashboard.AppManagement.Parking.SetParkingClusterStatus

type GetParkingOccupancy = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_OCCUPANCY" :> API.Types.Dashboard.AppManagement.Parking.GetParkingOccupancy

type GetParkingClusterRevenue =
  DashboardUserAuth
    'APP_BACKEND_MANAGEMENT
    "RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER_REVENUE"
    :> API.Types.Dashboard.AppManagement.Parking.GetParkingClusterRevenue

type VerifyParkingTicket = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/VERIFY_PARKING_TICKET" :> API.Types.Dashboard.AppManagement.Parking.VerifyParkingTicket

type CashBookParking = DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/CASH_BOOK_PARKING" :> API.Types.Dashboard.AppManagement.Parking.CashBookParking

handler :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = listParkingClusters merchantId city :<|> getParkingCityRevenue merchantId city :<|> getParkingCluster merchantId city :<|> setParkingClusterStatus merchantId city :<|> getParkingOccupancy merchantId city :<|> getParkingClusterRevenue merchantId city :<|> verifyParkingTicket merchantId city :<|> cashBookParking merchantId city

listParkingClusters :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Parking.ParkingClusterResp]))
listParkingClusters a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.AppManagement.Parking.listParkingClusters a3 a2
    )

getParkingCityRevenue :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Data.Time.Day -> (Data.Time.Day -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Parking.ParkingZoneRevenue]))))
getParkingCityRevenue a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Parking.getParkingCityRevenue a5 a4 a2 a1
    )

getParkingCluster :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Parking.ParkingClusterResp)))
getParkingCluster a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Parking.getParkingCluster a4 a3 a1
    )

setParkingClusterStatus :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> (API.Types.Dashboard.AppManagement.Parking.ParkingClusterStatusReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Parking.ParkingClusterResp))))
setParkingClusterStatus a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/SET_PARKING_CLUSTER_STATUS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Parking.setParkingClusterStatus a5 a4 a2 a1
    )

getParkingOccupancy :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Parking.ParkingOccupancyResp))))
getParkingOccupancy a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Parking.getParkingOccupancy a5 a4 a2 a1
    )

getParkingClusterRevenue :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> (Data.Time.Day -> (Data.Time.Day -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Parking.ParkingZoneRevenue])))))
getParkingClusterRevenue a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.Parking.getParkingClusterRevenue a6 a5 a3 a2 a1
    )

verifyParkingTicket :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> (API.Types.UI.TicketService.TicketServiceVerificationReq -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp))))))
verifyParkingTicket a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/VERIFY_PARKING_TICKET" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.Parking.verifyParkingTicket a7 a6 a4 a3 a2 a1
    )

cashBookParking :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> (API.Types.UI.TicketService.DirectTicketBookingReq -> Environment.FlowHandler API.Types.UI.TicketService.DirectTicketBookingResp)))))
cashBookParking a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PARKING/CASH_BOOK_PARKING" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.Parking.cashBookParking a6 a5 a3 a2 a1
    )
