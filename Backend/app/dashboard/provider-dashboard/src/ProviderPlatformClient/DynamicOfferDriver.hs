{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module ProviderPlatformClient.DynamicOfferDriver
  ( callDriverOfferBPP,
    callDynamicOfferDriverAppExotelApi,
  )
where

import "dynamic-offer-driver-app" API.Dashboard as BPP
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.Common.Exotel as Exotel
import qualified Dashboard.ProviderPlatform.Driver as Driver
import qualified Dashboard.ProviderPlatform.Driver.Registration as Registration
import qualified Dashboard.ProviderPlatform.DriverReferral as DriverReferral
import qualified Dashboard.ProviderPlatform.Issue as Issue
import qualified Dashboard.ProviderPlatform.Merchant as Merchant
import qualified Dashboard.ProviderPlatform.Message as Message
import qualified Dashboard.ProviderPlatform.Ride as Ride
import qualified Dashboard.ProviderPlatform.Volunteer as Volunteer
import qualified Data.ByteString.Lazy as LBS
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client
import "lib-dashboard" Tools.Metrics

data DriverOfferAPIs = DriverOfferAPIs
  { drivers :: DriversAPIs,
    rides :: RidesAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs,
    message :: MessageAPIs,
    volunteer :: VolunteerAPIs,
    driverReferral :: DriverReferralAPIs,
    issue :: IssueAPIs
  }

data DriversAPIs = DriversAPIs
  { driverDocumentsInfo :: Euler.EulerClient Driver.DriverDocumentsInfoRes,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverListRes,
    driverActivity :: Euler.EulerClient Driver.DriverActivityRes,
    enableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    blockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unblockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Driver.DriverIds -> Euler.EulerClient Driver.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverInfoRes,
    deleteDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    documentsList :: Id Driver.Driver -> Euler.EulerClient Registration.DocumentsListResponse,
    getDocument :: Id Driver.Image -> Euler.EulerClient Registration.GetDocumentResponse,
    uploadDocument :: Id Driver.Driver -> Registration.UploadDocumentReq -> Euler.EulerClient Registration.UploadDocumentResp,
    registerDL :: Id Driver.Driver -> Registration.RegisterDLReq -> Euler.EulerClient APISuccess,
    registerRC :: Id Driver.Driver -> Registration.RegisterRCReq -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkDL :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    endRCAssociation :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Driver.Driver -> Driver.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Driver.Driver -> Driver.AddVehicleReq -> Euler.EulerClient APISuccess,
    updateDriverName :: Id Driver.Driver -> Driver.UpdateDriverNameReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Ride.BookingStatus -> Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Money -> Euler.EulerClient Ride.RideListRes,
    rideStart :: Id Ride.Ride -> Ride.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Ride.Ride -> Ride.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Ride.Ride -> Ride.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.RideInfoRes,
    rideSync :: Id Ride.Ride -> Euler.EulerClient Ride.RideSyncRes,
    rideRoute :: Id Ride.Ride -> Euler.EulerClient Ride.RideRouteRes
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Merchant.MerchantUpdateReq -> Euler.EulerClient Merchant.MerchantUpdateRes,
    merchantCommonConfigUpdate :: Merchant.MerchantCommonConfigUpdateReq -> Euler.EulerClient APISuccess,
    driverPoolConfigUpdate :: Meters -> Merchant.DriverPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    driverPoolConfigCreate :: Meters -> Merchant.DriverPoolConfigCreateReq -> Euler.EulerClient APISuccess,
    driverIntelligentPoolConfigUpdate :: Merchant.DriverIntelligentPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    onboardingDocumentConfigUpdate :: Merchant.DocumentType -> Merchant.OnboardingDocumentConfigUpdateReq -> Euler.EulerClient APISuccess,
    onboardingDocumentConfigCreate :: Merchant.DocumentType -> Merchant.OnboardingDocumentConfigCreateReq -> Euler.EulerClient APISuccess,
    mapsServiceConfigUpdate :: Merchant.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    verificationServiceConfigUpdate :: Merchant.VerificationServiceConfigUpdateReq -> Euler.EulerClient APISuccess
  }

data DriverReferralAPIs = DriverReferralAPIs
  { updateReferralLinkPassword :: DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Euler.EulerClient APISuccess,
    linkDriverReferralCode :: (LBS.ByteString, DriverReferral.ReferralLinkReq) -> Euler.EulerClient DriverReferral.LinkReport
  }

data MessageAPIs = MessageAPIs
  { uploadFile :: (LBS.ByteString, Message.UploadFileRequest) -> Euler.EulerClient Message.UploadFileResponse,
    addLinkAsMedia :: Message.AddLinkAsMedia -> Euler.EulerClient Message.UploadFileResponse,
    addMessage :: Message.AddMessageRequest -> Euler.EulerClient Message.AddMessageResponse,
    sendMessage :: (LBS.ByteString, Message.SendMessageRequest) -> Euler.EulerClient APISuccess,
    messageList :: Maybe Int -> Maybe Int -> Euler.EulerClient Message.MessageListResponse,
    messageInfo :: Id Message.Message -> Euler.EulerClient Message.MessageInfoResponse,
    messageDeliveryInfo :: Id Message.Message -> Euler.EulerClient Message.MessageDeliveryInfoResponse,
    messageReceiverList :: Id Message.Message -> Maybe Text -> Maybe Message.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Euler.EulerClient Message.MessageReceiverListResponse
  }

data VolunteerAPIs = VolunteerAPIs
  { bookingInfo :: Text -> Euler.EulerClient Volunteer.BookingInfoResponse,
    assignCreateAndStartOtpRide :: Volunteer.AssignCreateAndStartOtpRideAPIReq -> Euler.EulerClient APISuccess
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe Issue.IssueStatus -> Maybe (Id Issue.IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id Issue.IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id Issue.IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id Issue.IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text
  }

mkDriverOfferAPIs :: CheckedShortId DM.Merchant -> Text -> DriverOfferAPIs
mkDriverOfferAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  let driverReferral = DriverReferralAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let message = MessageAPIs {..}
  let volunteer = VolunteerAPIs {..}
  let issue = IssueAPIs {..}
  DriverOfferAPIs {..}
  where
    driversClient
      :<|> ridesClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> messageClient
      :<|> driverReferralClient
      :<|> volunteerClient
      :<|> issueClient = clientWithMerchant (Proxy :: Proxy BPP.API') merchantId token

    driverDocumentsInfo
      :<|> listDrivers
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> blockDriver
      :<|> unblockDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> unlinkDL
      :<|> endRCAssociation
      :<|> updatePhoneNumber
      :<|> addVehicle
      :<|> updateDriverName
      :<|> ( documentsList
               :<|> getDocument
               :<|> uploadDocument
               :<|> registerDL
               :<|> registerRC
             ) =
        driversClient

    rideList
      :<|> rideStart
      :<|> rideEnd
      :<|> rideCancel
      :<|> rideInfo
      :<|> rideSync
      :<|> rideRoute = ridesClient

    stuckBookingsCancel = bookingsClient

    merchantUpdate
      :<|> merchantCommonConfigUpdate
      :<|> driverPoolConfigUpdate
      :<|> driverPoolConfigCreate
      :<|> driverIntelligentPoolConfigUpdate
      :<|> onboardingDocumentConfigUpdate
      :<|> onboardingDocumentConfigCreate
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate
      :<|> verificationServiceConfigUpdate = merchantClient

    updateReferralLinkPassword
      :<|> linkDriverReferralCode = driverReferralClient

    uploadFile
      :<|> addLinkAsMedia
      :<|> addMessage
      :<|> sendMessage
      :<|> messageList
      :<|> messageInfo
      :<|> messageDeliveryInfo
      :<|> messageReceiverList = messageClient

    bookingInfo
      :<|> assignCreateAndStartOtpRide = volunteerClient

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia = issueClient

callDriverOfferBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI DriverOfferAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (DriverOfferAPIs -> b) ->
  c
callDriverOfferBPP merchantId = callServerAPI @_ @m @r DRIVER_OFFER_BPP (mkDriverOfferAPIs merchantId) "callDriverOfferBPP"

newtype ExotelAPIs = ExotelAPIs
  { exotelHeartbeat :: Exotel.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
  }

mkDynamicOfferDriverAppExotelAPIs :: Text -> ExotelAPIs
mkDynamicOfferDriverAppExotelAPIs token = do
  ExotelAPIs {..}
  where
    exotelHeartbeat = Euler.client (Proxy :: Proxy BPP.ExotelAPI) token

callDynamicOfferDriverAppExotelApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI ExotelAPIs m r b c
  ) =>
  (ExotelAPIs -> b) ->
  c
callDynamicOfferDriverAppExotelApi = callServerAPI @_ @m @r DRIVER_OFFER_BPP mkDynamicOfferDriverAppExotelAPIs "callDynamicOfferDriverAppExotelApi"
