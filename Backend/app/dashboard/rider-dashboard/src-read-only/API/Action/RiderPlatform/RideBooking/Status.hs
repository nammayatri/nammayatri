{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Status
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Status
import qualified Domain.Action.RiderPlatform.RideBooking.Status
import qualified Domain.Types.FRFSTicketBooking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("status" :> GetStatusGetFRFSTicketStatus)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getStatusGetFRFSTicketStatus merchantId city

type GetStatusGetFRFSTicketStatus =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.STATUS / 'API.Types.Dashboard.RideBooking.Status.GET_STATUS_GET_FRFS_TICKET_STATUS)
      :> API.Types.Dashboard.RideBooking.Status.GetStatusGetFRFSTicketStatus
  )

getStatusGetFRFSTicketStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getStatusGetFRFSTicketStatus merchantShortId opCity apiTokenInfo bookingId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Status.getStatusGetFRFSTicketStatus merchantShortId opCity apiTokenInfo bookingId
