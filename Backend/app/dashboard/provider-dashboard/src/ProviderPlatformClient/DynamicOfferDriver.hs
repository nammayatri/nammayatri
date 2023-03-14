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
import qualified Dashboard.Common.Booking as Common
import qualified Dashboard.Common.Exotel as Common
import qualified Dashboard.ProviderPlatform.Driver as Common
import qualified Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Dashboard.ProviderPlatform.DriverReferral as Common
import qualified Dashboard.ProviderPlatform.Merchant as Common
import qualified Dashboard.ProviderPlatform.Message as Common
import qualified Dashboard.ProviderPlatform.Ride as Common
import qualified Dashboard.ProviderPlatform.Issue as Common
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
    driverReferral :: DriverReferralAPIs,
    issue :: IssueAPIs
  }

data DriversAPIs = DriversAPIs
  { driverDocumentsInfo :: Euler.EulerClient Common.DriverDocumentsInfoRes,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    blockDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    unblockDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Common.DriverInfoRes,
    deleteDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    documentsList :: Id Common.Driver -> Euler.EulerClient Common.DocumentsListResponse,
    getDocument :: Id Common.Image -> Euler.EulerClient Common.GetDocumentResponse,
    uploadDocument :: Id Common.Driver -> Common.UploadDocumentReq -> Euler.EulerClient Common.UploadDocumentResp,
    registerDL :: Id Common.Driver -> Common.RegisterDLReq -> Euler.EulerClient APISuccess,
    registerRC :: Id Common.Driver -> Common.RegisterRCReq -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Common.Driver -> Euler.EulerClient APISuccess,
    unlinkDL :: Id Common.Driver -> Euler.EulerClient APISuccess,
    endRCAssociation :: Id Common.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Common.Driver -> Common.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Common.Driver -> Common.AddVehicleReq -> Euler.EulerClient APISuccess,
    updateDriverName :: Id Common.Driver -> Common.UpdateDriverNameReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Common.BookingStatus -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Money -> Euler.EulerClient Common.RideListRes,
    rideStart :: Id Common.Ride -> Common.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Common.Ride -> Common.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Common.Ride -> Common.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Common.Ride -> Euler.EulerClient Common.RideInfoRes,
    rideSync :: Id Common.Ride -> Euler.EulerClient Common.RideSyncRes,
    rideRoute :: Id Common.Ride -> Euler.EulerClient Common.RideRouteRes
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Common.StuckBookingsCancelReq -> Euler.EulerClient Common.StuckBookingsCancelRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Common.MerchantUpdateReq -> Euler.EulerClient Common.MerchantUpdateRes,
    mapsServiceConfigUpdate :: Common.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Common.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Common.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Common.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    verificationServiceConfigUpdate :: Common.VerificationServiceConfigUpdateReq -> Euler.EulerClient APISuccess
  }

data DriverReferralAPIs = DriverReferralAPIs
  { updateReferralLinkPassword :: Common.ReferralLinkPasswordUpdateAPIReq -> Euler.EulerClient APISuccess,
    linkDriverReferralCode :: (LBS.ByteString, Common.ReferralLinkReq) -> Euler.EulerClient Common.LinkReport
  }

data MessageAPIs = MessageAPIs
  { uploadFile :: (LBS.ByteString, Common.UploadFileRequest) -> Euler.EulerClient Common.UploadFileResponse,
    addLinkAsMedia :: Common.AddLinkAsMedia -> Euler.EulerClient Common.UploadFileResponse,
    addMessage :: Common.AddMessageRequest -> Euler.EulerClient Common.AddMessageResponse,
    sendMessage :: (LBS.ByteString, Common.SendMessageRequest) -> Euler.EulerClient APISuccess,
    messageList :: Maybe Int -> Maybe Int -> Euler.EulerClient Common.MessageListResponse,
    messageInfo :: Id Common.Message -> Euler.EulerClient Common.MessageInfoResponse,
    messageDeliveryInfo :: Id Common.Message -> Euler.EulerClient Common.MessageDeliveryInfoResponse,
    messageReceiverList :: Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Euler.EulerClient Common.MessageReceiverListResponse
  }

data IssueAPIs = IssueAPIs
  { issueList :: Maybe Int -> Maybe Int -> Maybe Common.IssueStatus -> Maybe Text -> Maybe Text -> Euler.EulerClient Common.IssueReportListResponse,
    issueUpdate :: Id Common.IssueReport -> Common.IssueUpdateReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id Common.IssueReport -> Common.IssueAddCommentReq -> Euler.EulerClient APISuccess
  }

mkDriverOfferAPIs :: CheckedShortId DM.Merchant -> Text -> DriverOfferAPIs
mkDriverOfferAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  let driverReferral = DriverReferralAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let message = MessageAPIs {..}
  let issue = IssueAPIs {..}
  DriverOfferAPIs {..}
  where
    driversClient
      :<|> ridesClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> messageClient
      :<|> driverReferralClient
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
    
    issueList
      :<|> issueUpdate
      :<|> issueAddComment = issueClient

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
  { exotelHeartbeat :: Common.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
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
