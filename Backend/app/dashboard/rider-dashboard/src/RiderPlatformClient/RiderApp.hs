{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RiderPlatformClient.RiderApp
  ( callRiderApp,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Registration as CR
import qualified "rider-app" API.UI.Confirm as UC
import qualified "rider-app" API.UI.Search as SH
import qualified Dashboard.Common.Booking as Booking
import qualified "dashboard-helper-api" Dashboard.Common.Issue as Common
import qualified Dashboard.RiderPlatform.Customer as Customer
import qualified Dashboard.RiderPlatform.Merchant as Merchant
import qualified Dashboard.RiderPlatform.Ride as Ride
import qualified "rider-app" Domain.Action.Dashboard.IssueList as DI
import qualified Domain.Action.Dashboard.Ride as DCM
import qualified "rider-app" Domain.Action.UI.Booking as DBooking
import qualified "rider-app" Domain.Action.UI.Cancel as DCancel
import qualified "rider-app" Domain.Action.UI.Frontend as DFrontend
import qualified "rider-app" Domain.Action.UI.Maps as DMaps
import qualified "rider-app" Domain.Action.UI.Profile as DProfile
import qualified "rider-app" Domain.Action.UI.Quote as DQuote
import qualified "rider-app" Domain.Action.UI.Registration as DR
import qualified "rider-app" Domain.Action.UI.Select as DSelect
import qualified "rider-app" Domain.Types.Booking as SRB
import qualified "rider-app" Domain.Types.Booking.API as DB
import qualified "rider-app" Domain.Types.Estimate as DEstimate
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified "rider-app" Domain.Types.Person as DP
import qualified "rider-app" Domain.Types.Quote as Quote
import qualified "rider-app" Domain.Types.RegistrationToken as DTR
import qualified "rider-app" Domain.Types.SearchRequest as SSR
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant hiding (route)
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data AppBackendAPIs = AppBackendAPIs
  { customers :: CustomerAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs,
    rides :: RidesAPIs,
    rideBooking :: RideBookingAPIs,
    issues :: ListIssueAPIs
  }

data CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Customer.CustomerListRes,
    customerDelete :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerBlock :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerUnblock :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerInfo :: Id Customer.Customer -> Euler.EulerClient Customer.CustomerInfoRes
  }

data BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes,
    multipleBookingSync :: Booking.MultipleBookingSyncReq -> Euler.EulerClient Booking.MultipleBookingSyncResp
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Merchant.MerchantUpdateReq -> Euler.EulerClient APISuccess,
    serviceUsageConfig :: Euler.EulerClient Merchant.ServiceUsageConfigRes,
    mapsServiceConfigUpdate :: Merchant.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { shareRideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.ShareRideInfoRes,
    rideList :: Maybe Int -> Maybe Int -> Maybe Ride.BookingStatus -> Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient Ride.RideListRes,
    tripRoute :: Id Ride.Ride -> Double -> Double -> Euler.EulerClient Maps.GetRoutesResp,
    rideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.RideInfoRes,
    multipleRideCancel :: DCM.MultipleRideCancelReq -> Euler.EulerClient APISuccess,
    multipleRideSync :: Ride.MultipleRideSyncReq -> Euler.EulerClient Ride.MultipleRideSyncResp,
    ticketRideList :: Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Ride.TicketRideListRes
  }

data RideBookingAPIs = RideBookingAPIs
  { registration :: RegistrationAPIs,
    profile :: ProfileAPIs,
    search :: SearchAPIs,
    quote :: QuoteAPIs,
    select :: SelectAPIs,
    confirm :: ConfirmAPIs,
    booking :: BookingAPIs,
    maps :: MapsAPIs,
    flowStatus :: FlowStatusAPIs,
    cancel :: CancelBookingAPIs
  }

data RegistrationAPIs = RegistrationAPIs
  { auth :: CR.CustomerAuthReq -> Euler.EulerClient DR.AuthRes,
    verify :: Id DTR.RegistrationToken -> DR.AuthVerifyReq -> Euler.EulerClient DR.AuthVerifyRes,
    resend :: Id DTR.RegistrationToken -> Euler.EulerClient DR.ResendAuthRes,
    logout :: Id DP.Person -> Euler.EulerClient APISuccess
  }

data ProfileAPIs = ProfileAPIs
  { personDetails :: Id DP.Person -> Euler.EulerClient DProfile.ProfileRes,
    updatePerson :: Id DP.Person -> DProfile.UpdateProfileReq -> Euler.EulerClient APISuccess
  }

newtype SearchAPIs = SearchAPIs
  { rsearch :: Id DP.Person -> SH.SearchReq -> Euler.EulerClient SH.SearchRes
  }

newtype QuoteAPIs = QuoteAPIs
  { getQuote :: Id SSR.SearchRequest -> Id DP.Person -> Euler.EulerClient DQuote.GetQuotesRes
  }

data SelectAPIs = SelectAPIs
  { rSelect :: Id DP.Person -> Id DEstimate.Estimate -> Euler.EulerClient APISuccess,
    selectList :: Id DP.Person -> Id DEstimate.Estimate -> Euler.EulerClient DSelect.SelectListRes,
    selectResult :: Id DP.Person -> Id DEstimate.Estimate -> Euler.EulerClient DSelect.QuotesResultResponse,
    cancelSearch :: Id DP.Person -> Id DEstimate.Estimate -> Euler.EulerClient DSelect.CancelAPIResponse
  }

newtype ConfirmAPIs = ConfirmAPIs
  { rconfirm :: Id DP.Person -> Id Quote.Quote -> Maybe (Id DMPM.MerchantPaymentMethod) -> UC.ConfirmReq -> Euler.EulerClient UC.ConfirmRes
  }

data BookingAPIs = BookingAPIs
  { bookingStatus :: Id SRB.Booking -> Id DP.Person -> Euler.EulerClient DB.BookingAPIEntity,
    bookingList :: Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Euler.EulerClient DBooking.BookingListRes
  }

data MapsAPIs = MapsAPIs
  { autoComplete :: Id DP.Person -> DMaps.AutoCompleteReq -> Euler.EulerClient DMaps.AutoCompleteResp,
    getPlaceDetails :: Id DP.Person -> DMaps.GetPlaceDetailsReq -> Euler.EulerClient DMaps.GetPlaceDetailsResp,
    getPlaceName :: Id DP.Person -> DMaps.GetPlaceNameReq -> Euler.EulerClient DMaps.GetPlaceNameResp
  }

data FlowStatusAPIs = FlowStatusAPIs
  { personFlowStatus :: Id DP.Person -> Maybe Bool -> Euler.EulerClient DFrontend.GetPersonFlowStatusRes,
    notifyEvent :: Id DP.Person -> DFrontend.NotifyEventReq -> Euler.EulerClient DFrontend.NotifyEventResp
  }

newtype CancelBookingAPIs = CancelBookingAPIs
  { cancelBooking :: Id SRB.Booking -> Id DP.Person -> DCancel.CancelReq -> Euler.EulerClient APISuccess
  }

data ListIssueAPIs = ListIssueAPIs
  { listIssue :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient DI.IssueListRes,
    ticketStatusCallBack :: Common.TicketStatusCallBackReq -> Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId token = do
  let customers = CustomerAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let rides = RidesAPIs {..}
  let registration = RegistrationAPIs {..}
  let profile = ProfileAPIs {..}
  let search = SearchAPIs {..}
  let select = SelectAPIs {..}
  let quote = QuoteAPIs {..}
  let confirm = ConfirmAPIs {..}
  let booking = BookingAPIs {..}
  let maps = MapsAPIs {..}
  let flowStatus = FlowStatusAPIs {..}
  let cancel = CancelBookingAPIs {..}
  let rideBooking = RideBookingAPIs {..}
  let issues = ListIssueAPIs {..}
  AppBackendAPIs {..}
  where
    customersClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> ridesClient
      :<|> rideBookingClient
      :<|> issueClient = clientWithMerchant (Proxy :: Proxy BAP.API') merchantId token

    customerList
      :<|> customerDelete
      :<|> customerBlock
      :<|> customerUnblock
      :<|> customerInfo = customersClient

    stuckBookingsCancel
      :<|> multipleBookingSync = bookingsClient

    shareRideInfo
      :<|> rideList
      :<|> tripRoute
      :<|> rideInfo
      :<|> multipleRideCancel
      :<|> multipleRideSync
      :<|> ticketRideList = ridesClient

    registrationClient
      :<|> profileClient
      :<|> searchClient
      :<|> quoteClient
      :<|> selectClient
      :<|> confirmClient
      :<|> bookingClient
      :<|> mapsClient
      :<|> flowStatusClient
      :<|> cancelBookingClient = rideBookingClient

    auth
      :<|> verify
      :<|> resend
      :<|> logout = registrationClient

    personDetails
      :<|> updatePerson = profileClient

    rsearch = searchClient

    getQuote = quoteClient

    rSelect
      :<|> selectList
      :<|> selectResult
      :<|> cancelSearch = selectClient

    rconfirm = confirmClient

    bookingStatus
      :<|> bookingList = bookingClient

    autoComplete
      :<|> getPlaceDetails
      :<|> getPlaceName = mapsClient

    personFlowStatus
      :<|> notifyEvent = flowStatusClient

    cancelBooking = cancelBookingClient

    merchantUpdate
      :<|> serviceUsageConfig
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

    listIssue
      :<|> ticketStatusCallBack = issueClient

callRiderApp ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (AppBackendAPIs -> b) ->
  c
callRiderApp merchantId = callServerAPI @_ @m @r APP_BACKEND (mkAppBackendAPIs merchantId) "callRiderApp"
