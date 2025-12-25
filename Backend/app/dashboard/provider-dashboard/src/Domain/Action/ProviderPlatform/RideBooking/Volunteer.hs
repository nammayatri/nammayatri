module Domain.Action.ProviderPlatform.RideBooking.Volunteer
  ( getVolunteerBooking,
    postVolunteerAssignStartOtpRide,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking
import qualified API.Types.Dashboard.RideBooking.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getVolunteerBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.Dashboard.RideBooking.Volunteer.BookingInfoResponse)
getVolunteerBooking merchantShortId opCity apiTokenInfo bookingOtp = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.volunteerDSL.getVolunteerBooking) bookingOtp

postVolunteerAssignStartOtpRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.Volunteer.AssignCreateAndStartOtpRideAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postVolunteerAssignStartOtpRide merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.volunteerDSL.postVolunteerAssignStartOtpRide) req
