{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.MultiModal (getMultiModalList, postMultiModalSendMessage, postMultiModalAddComment, getMultiModalGetComments) where

import qualified API.Types.Dashboard.RideBooking.MultiModal
import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Action.UI.Booking as DBooking
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import Domain.Types.EmptyDynamicParam
import qualified "this" Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as KEsqueleto
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import Servant
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TNotifications

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Maybe Text -> Environment.Flow Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList merchantShortId _opCity limit offset bookingOffset journeyOffset customerPhoneNo countryCode email fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType customerId = do
  m <- findMerchantByShortId merchantShortId
  DBooking.bookingListV2ByCustomerLookup m.id limit offset bookingOffset journeyOffset fromDate toDate Nothing [] rideStatus journeyStatus isPaymentSuccess bookingRequestType customerPhoneNo countryCode email customerId

postMultiModalSendMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.MultiModal.CustomerSendMessageReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalSendMessage _merchantShortId _opCity customerId req = do
  notifyCustomerFromDashboard customerId req
  pure Kernel.Types.APISuccess.Success

notifyCustomerFromDashboard ::
  (ServiceFlow m r, EsqDBFlow m r, KEsqueleto.EsqDBReplicaFlow m r) =>
  Text ->
  API.Types.Dashboard.RideBooking.MultiModal.CustomerSendMessageReq ->
  m ()
notifyCustomerFromDashboard customerId req = do
  person <- QPerson.findById (Kernel.Types.Id.Id customerId) >>= fromMaybeM (PersonNotFound customerId)
  case req.channel of
    API.Types.Dashboard.RideBooking.MultiModal.WHATSAPP -> pure ()
    API.Types.Dashboard.RideBooking.MultiModal.SMS -> pure ()
    API.Types.Dashboard.RideBooking.MultiModal.PUSH_NOTIFICATION -> do
      let notificationData =
            Notification.NotificationReq
              { category = Notification.TRIGGER_FCM,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.Person person.id.getId EmptyDynamicParam,
                body = req.message,
                title = req.title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
                ttl = Nothing,
                sound = Nothing
              }
      TNotifications.notifyPerson person.merchantId person.merchantOperatingCityId person.id notificationData Nothing

postMultiModalAddComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.MultiModal.CustomerCommentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalAddComment _merchantShortId _opCity customerId req = do
  person <- QPerson.findById (Kernel.Types.Id.Id customerId) >>= fromMaybeM (PersonDoesNotExist customerId)
  let existingComments = person.comments
  let updatedComments = case existingComments of
        Just existingComments' -> Just $ req.body : existingComments'
        Nothing -> Just [req.body]
  QPerson.updatePersonComments (Kernel.Types.Id.Id customerId) updatedComments
  pure Kernel.Types.APISuccess.Success

getMultiModalGetComments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow API.Types.Dashboard.RideBooking.MultiModal.CustomerCommentsResp)
getMultiModalGetComments _merchantShortId _opCity customerId = do
  person <- QPerson.findById (Kernel.Types.Id.Id customerId) >>= fromMaybeM (PersonDoesNotExist customerId)
  pure API.Types.Dashboard.RideBooking.MultiModal.CustomerCommentsResp {comments = person.comments, customerId = person.id}

instance FromHttpApiData [Domain.Types.Journey.JourneyStatus] where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . BSL.fromStrict $ bs

instance ToHttpApiData [Domain.Types.Journey.JourneyStatus] where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
