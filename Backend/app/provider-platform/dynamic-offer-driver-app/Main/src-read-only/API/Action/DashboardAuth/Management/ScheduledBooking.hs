{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.ScheduledBooking
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.ScheduledBooking
import qualified Domain.Action.Dashboard.Management.ScheduledBooking
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

type API = ("scheduledBooking" :> (GetScheduledBookingList :<|> GetScheduledBookingInfo :<|> GetScheduledBookingDriverDistance :<|> GetScheduledBookingNearbyDrivers :<|> PostScheduledBookingAssign :<|> PostScheduledBookingOpsNote :<|> PostScheduledBookingUnassign))

type GetScheduledBookingList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_LIST"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingList
  )

type GetScheduledBookingInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_INFO"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingInfo
  )

type GetScheduledBookingDriverDistance =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_DRIVER_DISTANCE"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingDriverDistance
  )

type GetScheduledBookingNearbyDrivers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_NEARBY_DRIVERS"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingNearbyDrivers
  )

type PostScheduledBookingAssign =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_ASSIGN"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.PostScheduledBookingAssign
  )

type PostScheduledBookingOpsNote =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_OPS_NOTE"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.PostScheduledBookingOpsNote
  )

type PostScheduledBookingUnassign =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_UNASSIGN"
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.PostScheduledBookingUnassign
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getScheduledBookingList merchantId city :<|> getScheduledBookingInfo merchantId city :<|> getScheduledBookingDriverDistance merchantId city :<|> getScheduledBookingNearbyDrivers merchantId city :<|> postScheduledBookingAssign merchantId city :<|> postScheduledBookingOpsNote merchantId city :<|> postScheduledBookingUnassign merchantId city

getScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.ScheduledBooking.AssignmentStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingListRes)
getScheduledBookingList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingList a8 a7 a5 a4 a3 a2 a1

getScheduledBookingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes)
getScheduledBookingInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingInfo a4 a3 a1

getScheduledBookingDriverDistance :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.DriverDistanceRes)
getScheduledBookingDriverDistance a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingDriverDistance a4 a3 a1

getScheduledBookingNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.NearbyDriversRes)
getScheduledBookingNearbyDrivers a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingNearbyDrivers a5 a4 a2 a1

postScheduledBookingAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.AssignDriverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingAssign a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingAssign a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

postScheduledBookingUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingUnassign a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingUnassign a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postScheduledBookingOpsNote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.OpsNoteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingOpsNote a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingOpsNote a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
