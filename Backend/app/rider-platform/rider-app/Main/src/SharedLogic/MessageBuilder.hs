{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MessageBuilder
  ( BuildSendOTPMessageReq (..),
    BuildSOSAlertMessageReq (..),
    buildSendOTPMessage,
    BuildSendBookingOTPMessageReq (..),
    buildSendBookingOTPMessage,
    BuildGenericMessageReq (..),
    buildGenericMessage,
    buildSOSAlertMessage,
    BuildMarkRideAsSafeMessageReq (..),
    buildMarkRideAsSafeMessage,
    BuildFollowRideMessageReq (..),
    buildFollowRideStartedMessage,
    BuildAddedAsEmergencyContactMessageReq (..),
    buildAddedAsEmergencyContactMessage,
    BuildTicketBookingCancelledMessageReq (..),
    buildTicketBookingCancelled,
    BuildFRFSTicketBookedMessageReq (..),
    buildFRFSTicketBookedMessage,
    BuildSendRideEndOTPMessageReq (..),
    buildSendRideEndOTPMessage,
    shortenTrackingUrl,
    BuildDeliveryMessageReq (..),
    DeliveryMessageRequestType (..),
    buildDeliveryDetailsMessage,
    buildFRFSTicketCancelMessage,
    BuildFRFSTicketCancelMessageReq (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrganization as DPO
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import Tools.Error
import qualified Tools.SMS as Sms
import qualified UrlShortner.Common as UrlShortner

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

type BuildMessageFlow m r =
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    CacheFlow m r
  )

type SmsReqBuilder = Text -> Sms.SendSMSReq

buildSendSmsReq :: BuildMessageFlow m r => DMM.MerchantMessage -> [(Text, Text)] -> m SmsReqBuilder
buildSendSmsReq merchantMessage vars = do
  smsCfg <- asks (.smsCfg)
  let smsBody = foldl' (\msg (findKey, replaceVal) -> T.replace (templateText findKey) replaceVal msg) merchantMessage.message vars
      sender = fromMaybe smsCfg.sender merchantMessage.senderHeader
  return $ \phoneNumber -> Sms.SendSMSReq {..}

data BuildSendOTPMessageReq = BuildSendOTPMessageReq
  { otp :: Text,
    hash :: Text
  }
  deriving (Generic)

buildSendOTPMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildSendOTPMessageReq -> m SmsReqBuilder
buildSendOTPMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_OTP Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_OTP))
  buildSendSmsReq merchantMessage [("otp", req.otp), ("hash", req.hash)]

data BuildSendBookingOTPMessageReq = BuildSendBookingOTPMessageReq
  { otp :: Text,
    amount :: Text
  }
  deriving (Generic)

buildSendBookingOTPMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildSendBookingOTPMessageReq -> m SmsReqBuilder
buildSendBookingOTPMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_BOOKING_OTP Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_BOOKING_OTP))
  buildSendSmsReq merchantMessage [("otp", req.otp), ("amount", req.amount)]

newtype BuildSendRideEndOTPMessageReq = BuildSendRideEndOTPMessageReq
  { otp :: Text
  }
  deriving (Generic)

buildSendRideEndOTPMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildSendRideEndOTPMessageReq -> m SmsReqBuilder
buildSendRideEndOTPMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_RIDE_END_OTP Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_RIDE_END_OTP))
  buildSendSmsReq merchantMessage [("otp", req.otp)]

data BuildGenericMessageReq = BuildGenericMessageReq {}
  deriving (Generic)

buildGenericMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> DMM.MessageKey -> BuildGenericMessageReq -> m SmsReqBuilder
buildGenericMessage merchantOpCityId messageKey _ = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOpCityId messageKey Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let jsonData = merchantMessage.jsonData
  buildSendSmsReq merchantMessage [("var1", fromMaybe "" jsonData.var1), ("var2", fromMaybe "" jsonData.var2), ("var3", fromMaybe "" jsonData.var3)]

data BuildSOSAlertMessageReq = BuildSOSAlertMessageReq
  { userName :: Text,
    rideLink :: Text,
    rideEndTime :: Maybe Text,
    isRideEnded :: Bool
  }
  deriving (Generic)

buildSOSAlertMessage :: (BuildMessageFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Id DMOC.MerchantOperatingCity -> BuildSOSAlertMessageReq -> m SmsReqBuilder
buildSOSAlertMessage merchantOperatingCityId req = do
  shortenedTrackingUrl <- shortenTrackingUrl req.rideLink
  let messageKey = if req.isRideEnded then DMM.POST_RIDE_SOS else DMM.SEND_SOS_ALERT
      smsParams = if req.isRideEnded then [("userName", req.userName), ("rideEndTime", fromMaybe "" req.rideEndTime)] else [("userName", req.userName), ("rideLink", shortenedTrackingUrl)]
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId messageKey Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId $ show messageKey)
  buildSendSmsReq merchantMessage smsParams

newtype BuildMarkRideAsSafeMessageReq = BuildMarkRideAsSafeMessageReq
  { userName :: Text
  }
  deriving (Generic)

buildMarkRideAsSafeMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildMarkRideAsSafeMessageReq -> m SmsReqBuilder
buildMarkRideAsSafeMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.MARK_RIDE_AS_SAFE Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.MARK_RIDE_AS_SAFE))
  buildSendSmsReq merchantMessage [("userName", req.userName)]

data BuildFollowRideMessageReq = BuildFollowRideMessageReq
  { userName :: Text,
    rideLink :: Text
  }
  deriving (Generic)

buildFollowRideStartedMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildFollowRideMessageReq -> m SmsReqBuilder
buildFollowRideStartedMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.FOLLOW_RIDE Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FOLLOW_RIDE))
  buildSendSmsReq merchantMessage [("userName", req.userName), ("rideLink", req.rideLink)]

data BuildAddedAsEmergencyContactMessageReq = BuildAddedAsEmergencyContactMessageReq
  { userName :: Text,
    appUrl :: Text
  }
  deriving (Generic)

buildAddedAsEmergencyContactMessage :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildAddedAsEmergencyContactMessageReq -> m SmsReqBuilder
buildAddedAsEmergencyContactMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.ADDED_AS_EMERGENCY_CONTACT Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.ADDED_AS_EMERGENCY_CONTACT))
  buildSendSmsReq merchantMessage [("userName", req.userName), ("appUrl", req.appUrl)]

data BuildTicketBookingCancelledMessageReq = BuildTicketBookingCancelledMessageReq
  { personName :: Text,
    categoryName :: Text
  }
  deriving (Generic)

buildTicketBookingCancelled :: BuildMessageFlow m r => Id DMOC.MerchantOperatingCity -> BuildTicketBookingCancelledMessageReq -> m SmsReqBuilder
buildTicketBookingCancelled merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.TICKET_BOOKING_CANCELLED Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.TICKET_BOOKING_CANCELLED))
  buildSendSmsReq merchantMessage [("personName", req.personName), ("categoryName", req.categoryName)]

data BuildFRFSTicketBookedMessageReq = BuildFRFSTicketBookedMessageReq
  { countOfTickets :: Int,
    bookingId :: Id DFTB.FRFSTicketBooking
  }
  deriving (Generic, Show)

buildFRFSTicketBookedMessage :: (EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Id DPO.PartnerOrganization -> BuildFRFSTicketBookedMessageReq -> m (Maybe Text)
buildFRFSTicketBookedMessage pOrgId req = do
  smsPOCfg <- do
    pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.TICKET_SMS >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.TICKET_SMS)
    DPOC.getTicketSMSConfig pOrgCfg.config

  forM smsPOCfg.template $
    \msg -> do
      let ticketPlural = bool "tickets are" "ticket is" $ req.countOfTickets == 1
          baseUrl = smsPOCfg.publicUrl & T.replace (templateText "FRFS_BOOKING_ID") req.bookingId.getId
          shortUrlReq =
            UrlShortner.GenerateShortUrlReq
              { baseUrl,
                customShortCode = Nothing,
                shortCodeLength = Nothing,
                expiryInHours = Nothing,
                urlCategory = UrlShortner.METRO_TICKET_BOOKING
              }
      res <- UrlShortner.generateShortUrl shortUrlReq
      let url = res.shortUrl
      logDebug $ "Generated short url: " <> url
      pure $
        msg
          & T.replace (templateText "TICKET_PLURAL") ticketPlural
          & T.replace (templateText "URL") url

data BuildFRFSTicketCancelMessageReq = BuildFRFSTicketCancelMessageReq
  { countOfTickets :: Int,
    bookingId :: Id DFTB.FRFSTicketBooking
  }
  deriving (Generic, Show)

buildFRFSTicketCancelMessage :: (BuildMessageFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Id DMOC.MerchantOperatingCity -> Id DPO.PartnerOrganization -> BuildFRFSTicketCancelMessageReq -> m SmsReqBuilder
buildFRFSTicketCancelMessage merchantOperatingCityId pOrgId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.METRO_TICKET_BOOKING_CANCELLED Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.METRO_TICKET_BOOKING_CANCELLED))
  smsPOCfg <- do
    pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.TICKET_SMS >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.TICKET_SMS)
    DPOC.getTicketSMSConfig pOrgCfg.config
  let ticketPlural = bool "tickets" "ticket" $ req.countOfTickets == 1
      baseUrl = smsPOCfg.publicUrl & T.replace (templateText "FRFS_BOOKING_ID") req.bookingId.getId
      shortUrlReq =
        UrlShortner.GenerateShortUrlReq
          { baseUrl,
            customShortCode = Nothing,
            shortCodeLength = Nothing,
            expiryInHours = Nothing,
            urlCategory = UrlShortner.METRO_TICKET_BOOKING
          }
  res <- UrlShortner.generateShortUrl shortUrlReq
  let url = res.shortUrl
  logDebug $ "Generated short url: " <> url
  buildSendSmsReq merchantMessage [("URL", url), ("TICKET_PLURAL", ticketPlural)]

shortenTrackingUrl :: (EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Text -> m Text
shortenTrackingUrl url = do
  let shortUrlReq =
        UrlShortner.GenerateShortUrlReq
          { baseUrl = url,
            customShortCode = Nothing,
            shortCodeLength = Nothing,
            expiryInHours = Just 24,
            urlCategory = UrlShortner.RIDE_TRACKING
          }
  res <- UrlShortner.generateShortUrl shortUrlReq
  return res.shortUrl

data DeliveryMessageRequestType = SenderReq | ReceiverReq

data BuildDeliveryMessageReq = BuildDeliveryMessageReq
  { driverName :: Text,
    driverNumber :: Text,
    trackingUrl :: Maybe Text,
    senderName :: Text,
    receiverName :: Text,
    appUrl :: Text,
    otp :: Text,
    hasEnded :: Bool,
    pickedUp :: Bool,
    deliveryMessageType :: DeliveryMessageRequestType
  }

buildDeliveryDetailsMessage :: (BuildMessageFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Id DMOC.MerchantOperatingCity -> BuildDeliveryMessageReq -> m SmsReqBuilder
buildDeliveryDetailsMessage merchantOperatingCityId req = do
  let merchantMessageKey = case req.deliveryMessageType of
        SenderReq -> bool DMM.SMS_DELIVERY_DETAILS_SENDER DMM.POST_DELIVERY_SENDER req.hasEnded
        ReceiverReq -> bool DMM.PRE_PICKUP_DELIVERY_RECEIVER DMM.SMS_DELIVERY_DETAILS_RECEIVER req.pickedUp
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId merchantMessageKey Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show merchantMessageKey))
  shortTrackingUrl <- maybe (pure mempty) shortenTrackingUrl req.trackingUrl
  buildSendSmsReq
    merchantMessage
    [ ("driverName", req.driverName),
      ("driverNumber", req.driverNumber),
      ("trackingUrl", shortTrackingUrl),
      ("senderName", req.senderName),
      ("receiverName", req.receiverName),
      ("appUrl", req.appUrl),
      ("otp", req.otp)
    ]
