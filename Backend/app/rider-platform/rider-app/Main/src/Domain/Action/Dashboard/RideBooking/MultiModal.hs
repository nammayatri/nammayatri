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
import qualified Domain.Types.Journey as DJ
import Domain.Types.EmptyDynamicParam
import qualified "this" Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Notification as Notification
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import Kernel.Utils.Common
import Lib.JourneyModule.Base (generateJourneyInfoResponse, getAllLegsInfo)
import Lib.JourneyModule.Types (GetStateFlow)
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.MessageBuilder (buildSendSmsReq)
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.Queries.JourneyExtra as SQJ
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TNotifications
import Tools.SMS as Sms hiding (Success)
import qualified Tools.Whatsapp as Whatsapp

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Maybe Text -> Environment.Flow Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList merchantShortId _opCity limit offset bookingOffset journeyOffset customerPhoneNo countryCode email fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType customerId = do
  m <- findMerchantByShortId merchantShortId
  -- Resolve person once; feed directly into bookingListV2 to avoid a second phone-hash lookup in bookingListV2ByCustomerLookup
  personId <- resolvePersonId customerId customerPhoneNo countryCode email m.id
  bookingResult <- DBooking.bookingListV2 (personId, m.id) limit offset bookingOffset journeyOffset Nothing fromDate toDate [] [] (fromMaybe [] rideStatus) (fromMaybe [] journeyStatus) isPaymentSuccess bookingRequestType Nothing Nothing
  additionalEntities <-
    if isPaymentSuccess == Just True
      then do
        let mbFromDate' = millisecondsToUTC <$> fromDate
            mbToDate' = millisecondsToUTC <$> toDate
        pendingJourneys <- SQJ.findJourneysWithPendingFRFSBooking personId Nothing Nothing mbFromDate' mbToDate'
        fmap catMaybes . forM pendingJourneys $ \j ->
          ( do
              legsInfo <- getAllLegsInfo j.riderId j.id
              if null legsInfo
                then logDebug ("getMultiModalList: No legs for " <> show j.id) >> return Nothing
                else Just . DBooking.MultiModalRide <$> generateJourneyInfoResponse j legsInfo
          )
            `catch` (\(e :: SomeException) -> logDebug ("getMultiModalList: build failed for " <> show j.id <> ": " <> show e) >> return Nothing)
      else return []
  pure (bookingResult :: DBooking.BookingListResV2) {DBooking.list = bookingResult.list ++ additionalEntities}
  where
    resolvePersonId mbCustomerId mbMobileNo mbCountryCode mbEmail merchantId =
      case mbCustomerId of
        Just cid -> pure $ Kernel.Types.Id.Id cid
        Nothing -> case mbMobileNo of
          Just mobileNo -> do
            mobileNoHash <- getDbHash mobileNo
            mbPerson <- QPerson.findByMobileNumberAndMerchantId (fromMaybe "+91" mbCountryCode) mobileNoHash merchantId
            case mbPerson of
              Just person -> pure person.id
              Nothing -> lookupByEmail mbEmail merchantId
          Nothing -> lookupByEmail mbEmail merchantId

    lookupByEmail mbEmail merchantId =
      case mbEmail of
        Just em -> do
          person <- QPerson.findByEmailAndMerchantId merchantId em >>= fromMaybeM (InternalError "Person with given email does not exist")
          pure person.id
        Nothing -> throwError $ InternalError "No Person Found"

postMultiModalSendMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.MultiModal.CustomerSendMessageReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalSendMessage _merchantShortId _opCity customerId req = do
  notifyCustomerFromDashboard customerId req
  pure Kernel.Types.APISuccess.Success

notifyCustomerFromDashboard ::
  Text ->
  API.Types.Dashboard.RideBooking.MultiModal.CustomerSendMessageReq ->
  Environment.Flow ()
notifyCustomerFromDashboard customerId req = do
  person <- QPerson.findById (Kernel.Types.Id.Id customerId) >>= fromMaybeM (PersonNotFound customerId)
  case req.channel of
    API.Types.Dashboard.RideBooking.MultiModal.WHATSAPP -> do
      whenJust req.messageKey $ \messageKey -> do
        mbMerchantMessage <- QMM.findByMerchantOperatingCityIdAndMessageKey person.merchantOperatingCityId messageKey Nothing
        whenJust mbMerchantMessage $ \merchantMessage -> do
          mbPhoneNumber <- decrypt `mapM` person.mobileNumber
          whenJust mbPhoneNumber $ \phoneNumber -> do
            let variables = map (Just . snd) (fromMaybe [] req.variables)
            void $ Whatsapp.whatsAppSendMessageWithTemplateIdAPI person.merchantId person.merchantOperatingCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId variables Nothing Nothing)
    API.Types.Dashboard.RideBooking.MultiModal.SMS -> do
      whenJust req.messageKey $ \messageKey -> do
        mbMerchantMessage <- QMM.findByMerchantOperatingCityIdAndMessageKey person.merchantOperatingCityId messageKey Nothing
        whenJust mbMerchantMessage $ \merchantMessage -> do
          mbPhoneNumber <- decrypt `mapM` person.mobileNumber
          whenJust mbPhoneNumber $ \phoneNumber -> do
            let countryCode = fromMaybe "+91" person.mobileCountryCode
                phoneNumberWithCountryCode = countryCode <> phoneNumber
            withLogTag ("sending SMS" <> Kernel.Types.Id.getId person.id) $ do
              buildSmsReq <- buildSendSmsReq merchantMessage (fromMaybe [] req.variables)
              Sms.sendSMS person.merchantId person.merchantOperatingCityId (buildSmsReq phoneNumberWithCountryCode)
                >>= Sms.checkSmsResult
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
