{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module RiderPlatformClient.RiderApp.Operations
  ( callRiderAppOperations,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.RiderPlatform.Customer as Customer
import qualified Dashboard.RiderPlatform.Merchant as Merchant
import qualified Dashboard.RiderPlatform.Ride as Ride
import qualified "rider-app" Domain.Action.Dashboard.IssueList as DI
import qualified Domain.Action.Dashboard.Ride as DCM
import qualified "rider-app" Domain.Action.UI.TicketService as DTB
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified "rider-app" Domain.Types.TicketBookingService as DTB
import qualified "rider-app" Domain.Types.TicketPlace as DTB
import qualified "rider-app" Domain.Types.TicketService as DTB
import qualified EulerHS.Types as Euler
import qualified IssueManagement.Common as DIssue
import IssueManagement.Common.Dashboard.Issue as Issue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
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
    issues :: ListIssueAPIs,
    issuesV2 :: IssueAPIs,
    tickets :: TicketAPIs
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

data ListIssueAPIs = ListIssueAPIs
  { listIssue :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient DI.IssueListRes,
    ticketStatusCallBack :: Issue.TicketStatusCallBackReq -> Euler.EulerClient APISuccess
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe DIssue.IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text,
    ticketStatusCallBack_ :: Issue.TicketStatusCallBackReq -> Euler.EulerClient APISuccess
  }

data TicketAPIs = TicketAPIs
  { verifyBookingDetails :: Id DTB.TicketService -> ShortId DTB.TicketBookingService -> Euler.EulerClient DTB.TicketServiceVerificationResp,
    getServices :: Id DTB.TicketPlace -> Euler.EulerClient [DTB.TicketServiceResp],
    updateSeatManagement :: DTB.TicketBookingUpdateSeatsReq -> Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId city token = do
  let customers = CustomerAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let rides = RidesAPIs {..}
  let issues = ListIssueAPIs {..}
  let issuesV2 = IssueAPIs {..}
  let tickets = TicketAPIs {..}
  AppBackendAPIs {..}
  where
    customersClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> ridesClient
      :<|> issueClient
      :<|> issueV2Client
      :<|> ticketsClient = clientWithMerchantAndCity (Proxy :: Proxy BAP.OperationsAPI) merchantId city token

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

    merchantUpdate
      :<|> serviceUsageConfig
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

    listIssue
      :<|> ticketStatusCallBack = issueClient

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia
      :<|> ticketStatusCallBack_ = issueV2Client

    verifyBookingDetails
      :<|> getServices
      :<|> updateSeatManagement = ticketsClient

callRiderAppOperations ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (AppBackendAPIs -> b) ->
  c
callRiderAppOperations merchantId city = callServerAPI @_ @m @r APP_BACKEND_MANAGEMENT (mkAppBackendAPIs merchantId city) "callRiderAppOperations"
