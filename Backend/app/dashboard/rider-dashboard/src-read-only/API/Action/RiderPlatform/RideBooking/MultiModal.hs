{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.MultiModal
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.MultiModal
import qualified Domain.Action.RiderPlatform.RideBooking.MultiModal
import qualified "rider-app" Domain.Action.UI.Booking
import qualified "rider-app" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "rider-app" Domain.Types.Journey
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("multiModal" :> GetMultiModalList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMultiModalList merchantId city

type GetMultiModalList =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MULTI_MODAL / 'API.Types.Dashboard.RideBooking.MultiModal.GET_MULTI_MODAL_LIST)
      :> API.Types.Dashboard.RideBooking.MultiModal.GetMultiModalList
  )

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList merchantShortId opCity apiTokenInfo limit offset bookingOffset journeyOffset customerPhoneNo countryCode email fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.MultiModal.getMultiModalList merchantShortId opCity apiTokenInfo limit offset bookingOffset journeyOffset customerPhoneNo countryCode email fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType customerId
