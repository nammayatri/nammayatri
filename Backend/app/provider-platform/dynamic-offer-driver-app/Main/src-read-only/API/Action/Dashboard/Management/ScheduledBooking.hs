{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.ScheduledBooking
  ( API.Types.ProviderPlatform.Management.ScheduledBooking.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.ScheduledBooking.API)
handler merchantId city = getScheduledBookingList merchantId city :<|> getScheduledBookingInfo merchantId city :<|> getScheduledBookingDriverDistance merchantId city :<|> getScheduledBookingNearbyDrivers merchantId city :<|> postScheduledBookingAssign merchantId city :<|> postScheduledBookingUnassign merchantId city :<|> postScheduledBookingOpsNote merchantId city

getScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.ScheduledBooking.AssignmentStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingListRes)
getScheduledBookingList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingList a7 a6 a5 a4 a3 a2 a1

getScheduledBookingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes)
getScheduledBookingInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingInfo a3 a2 a1

getScheduledBookingDriverDistance :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.DriverDistanceRes)
getScheduledBookingDriverDistance a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingDriverDistance a3 a2 a1

getScheduledBookingNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.NearbyDriversRes)
getScheduledBookingNearbyDrivers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.getScheduledBookingNearbyDrivers a4 a3 a2 a1

postScheduledBookingAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.AssignDriverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingAssign a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingAssign a5 a4 a3 a2 a1

postScheduledBookingUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingUnassign a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingUnassign a4 a3 a2 a1

postScheduledBookingOpsNote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.OpsNoteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingOpsNote a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ScheduledBooking.postScheduledBookingOpsNote a5 a4 a3 a2 a1
