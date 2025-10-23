{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.RideBooking.MultiModal (getMultiModalList) where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" Domain.Action.UI.Booking
import qualified "rider-app" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "rider-app" Domain.Types.Journey
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Environment.Flow Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList merchantShortId opCity apiTokenInfo customerId limit offset bookingOffset journeyOffset fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.multiModalDSL.getMultiModalList) customerId limit offset bookingOffset journeyOffset fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType
