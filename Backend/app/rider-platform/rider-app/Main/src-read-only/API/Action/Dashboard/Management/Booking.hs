{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.Booking 
( API.Types.RiderPlatform.Management.Booking.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Booking
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Dashboard.Common.Booking
import qualified API.Types.RiderPlatform.Management.Booking



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Booking.API)
handler merchantId city = postBookingCancelAllStuck merchantId city :<|> postBookingSyncMultiple merchantId city
postBookingCancelAllStuck :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Booking.StuckBookingsCancelReq -> Environment.FlowHandler Dashboard.Common.Booking.StuckBookingsCancelRes)
postBookingCancelAllStuck a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Booking.postBookingCancelAllStuck a3 a2 a1
postBookingSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Booking.MultipleBookingSyncReq -> Environment.FlowHandler Dashboard.Common.Booking.MultipleBookingSyncResp)
postBookingSyncMultiple a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Booking.postBookingSyncMultiple a3 a2 a1



