{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.OpenMeter
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.OpenMeter
import qualified Control.Lens
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.OpenMeter
import qualified Domain.Types.Merchant
import qualified Domain.Types.OpenMeterRide
import qualified Domain.Types.Person
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

type API = ("openMeter" :> (GetOpenMeterRideList :<|> GetOpenMeterRideInfo))

type GetOpenMeterRideList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OPEN_METER/GET_OPEN_METER_RIDE_LIST" :> API.Types.RiderPlatform.Management.OpenMeter.GetOpenMeterRideList)

type GetOpenMeterRideInfo = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/OPEN_METER/GET_OPEN_METER_RIDE_INFO" :> API.Types.RiderPlatform.Management.OpenMeter.GetOpenMeterRideInfo)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getOpenMeterRideList merchantId city :<|> getOpenMeterRideInfo merchantId city

getOpenMeterRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.OpenMeterRide.OpenMeterRideStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.RiderPlatform.Management.OpenMeter.OpenMeterRideListRes)
getOpenMeterRideList a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a7 a9 a8
        Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.OpenMeter.getOpenMeterRideList a9 a8 a6 a5 a4 a3 a2 a1
    )

getOpenMeterRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> Environment.FlowHandler API.Types.RiderPlatform.Management.OpenMeter.OpenMeterRideInfoRes)
getOpenMeterRideInfo a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.OpenMeter.getOpenMeterRideInfo a4 a3 a1
    )
