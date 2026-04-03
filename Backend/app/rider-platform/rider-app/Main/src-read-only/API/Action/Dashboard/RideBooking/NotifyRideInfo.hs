{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.RideBooking.NotifyRideInfo 
( API.Types.Dashboard.RideBooking.NotifyRideInfo.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.RideBooking.NotifyRideInfo
import qualified "this" Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.Dashboard.RideBooking.NotifyRideInfo
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.NotifyRideInfo.API)
handler merchantId city = postNotifyRideInfoNotifyRideInfo merchantId city
postNotifyRideInfoNotifyRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.NotifyRideInfo.NotifyRideInfoRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotifyRideInfoNotifyRideInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.NotifyRideInfo.postNotifyRideInfoNotifyRideInfo a4 a3 a2 a1



