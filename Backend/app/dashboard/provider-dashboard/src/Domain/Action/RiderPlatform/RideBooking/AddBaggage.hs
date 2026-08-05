module Domain.Action.RiderPlatform.RideBooking.AddBaggage
  ( postAddBaggageConfirm,
  )
where

import qualified API.Client.RiderPlatform.RideBooking
import qualified API.Types.Dashboard.RideBooking.Endpoints.AddBaggage as DashboardTypes
import qualified "rider-app" Domain.Types.Booking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postAddBaggageConfirm ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  DashboardTypes.AddBaggageConfirmReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAddBaggageConfirm merchantShortId opCity apiTokenInfo bookingId customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.addBaggageDSL.postAddBaggageConfirm) bookingId customerId req
