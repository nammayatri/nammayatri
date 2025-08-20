{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.NotifyRideInfo
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.NotifyRideInfo
import qualified Domain.Action.RiderPlatform.RideBooking.NotifyRideInfo
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("notifyRideInfo" :> PostNotifyRideInfoNotifyRideInfo)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNotifyRideInfoNotifyRideInfo merchantId city

type PostNotifyRideInfoNotifyRideInfo =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.NOTIFY_RIDE_INFO / 'API.Types.Dashboard.RideBooking.NotifyRideInfo.POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO)
      :> API.Types.Dashboard.RideBooking.NotifyRideInfo.PostNotifyRideInfoNotifyRideInfo
  )

postNotifyRideInfoNotifyRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.NotifyRideInfo.NotifyRideInfoRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNotifyRideInfoNotifyRideInfo merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.NotifyRideInfo.postNotifyRideInfoNotifyRideInfo merchantShortId opCity apiTokenInfo customerId req
