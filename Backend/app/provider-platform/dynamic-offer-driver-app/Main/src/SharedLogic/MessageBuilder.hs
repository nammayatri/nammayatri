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
    buildSendOTPMessage,
    WelcomeToPlatformMessageReq (..),
    buildWelcomeToPlatformMessage,
    buildSendAlternateNumberOTPMessage,
    BuildEndRideMessageReq (..),
    buildEndRideMessage,
    BuildOnboardingMessageReq (..),
    buildOnboardingMessage,
    BuildBookingMessageReq (..),
    buildBookingMessage,
    BuildCollectCashMessageReq (..),
    buildCollectCashMessage,
    BuildSendPaymentLinkReq (..),
    buildSendPaymentLink,
    BuildGenericMessageReq (..),
    buildGenericMessage,
    addBroadcastMessageToKafka,
    BuildFleetJoiningMessageReq (..),
    buildFleetJoiningMessage,
    BuildOperatorJoinAndDownloadAppMessageReq (..),
    buildOperatorJoinAndDownloadAppMessage,
    BuildOperatorJoiningMessageReq (..),
    buildOperatorJoiningMessage,
    BuildDownloadAppMessageReq (..),
    buildFleetJoinAndDownloadAppMessage,
    BuildFleetDeepLinkAuthMessage (..),
    buildFleetDeepLinkAuthMessage,
    BuildSendReceiptMessageReq (..),
    buildSendReceiptMessage,
    BuildOperatorDeepLinkAuthMessage (..),
    buildOperatorDeepLinkAuthMessage,
    BuildFleetLinkUnlinkSuccessMessageReq (..),
    buildFleetLinkSuccessMessage,
    buildFleetUnlinkSuccessMessage,
    BuildDriverPayoutMessageReq (..),
    buildDriverPayoutMessage,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Message as Message
import qualified Domain.Types.Person as P
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Streaming.Kafka.Commons
import Kernel.Streaming.Kafka.Producer
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.Queries.MessageTranslation as MTQuery
import Tools.Error
import qualified UrlShortner.Common as UrlShortner

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

data BuildSendPaymentLinkReq = BuildSendPaymentLinkReq
  { paymentLink :: Text,
    amount :: Text
  }
  deriving (Generic)

buildSendPaymentLink :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSendPaymentLinkReq -> m (Maybe Text, Text, Text, Maybe Text)
buildSendPaymentLink merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.SEND_PAYMENT_LINK Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.SEND_PAYMENT_LINK))
  let msg =
        merchantMessage.message
          & T.replace (templateText "paymentLink") req.paymentLink
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildSendOTPMessageReq = BuildSendOTPMessageReq
  { otp :: Text,
    hash :: Text
  }
  deriving (Generic)

buildSendOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSendOTPMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildSendOTPMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.SEND_OTP Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.SEND_OTP))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "hash") req.hash

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

newtype WelcomeToPlatformMessageReq = WelcomeToPlatformMessageReq
  { orgName :: Text
  }
  deriving (Generic)

buildWelcomeToPlatformMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> WelcomeToPlatformMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildWelcomeToPlatformMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.WELCOME_TO_PLATFORM Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.WELCOME_TO_PLATFORM))
  let msg =
        merchantMessage.message
          & T.replace (templateText "orgName") req.orgName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

buildSendAlternateNumberOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSendOTPMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildSendAlternateNumberOTPMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.ALTERNATE_NUMBER_OTP Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.ALTERNATE_NUMBER_OTP))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "hash") req.hash

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildEndRideMessageReq = BuildEndRideMessageReq
  { rideAmount :: Text,
    rideShortId :: Text
  }
  deriving (Generic)

buildEndRideMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildEndRideMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildEndRideMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.END_RIDE_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.END_RIDE_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "rideAmount") req.rideAmount
          & T.replace (templateText "rideId") req.rideShortId

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildOnboardingMessageReq = BuildOnboardingMessageReq {}
  deriving (Generic)

buildOnboardingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildOnboardingMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildOnboardingMessage merchantOpCityId _ = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.ONBOARDING_YATRI_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.ONBOARDING_YATRI_MESSAGE))

  pure (merchantMessage.senderHeader, merchantMessage.message, merchantMessage.templateId, merchantMessage.messageType)

data BuildBookingMessageReq = BuildBookingMessageReq
  { otp :: Text,
    amount :: Text
  }
  deriving (Generic)

buildBookingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildBookingMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildBookingMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.BOOKING_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.BOOKING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

newtype BuildCollectCashMessageReq = BuildCollectCashMessageReq
  { amount :: Text
  }
  deriving (Generic)

buildCollectCashMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildCollectCashMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildCollectCashMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.CASH_COLLECTED_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.CASH_COLLECTED_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildDriverPayoutMessageReq = BuildDriverPayoutMessageReq
  { payoutAmount :: Text,
    bookingId :: Maybe Text
  }
  deriving (Generic)

buildDriverPayoutMessage ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  BuildDriverPayoutMessageReq ->
  m (Maybe Text, Text, Text, Maybe Text)
buildDriverPayoutMessage merchantOpCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.DRIVER_PAYOUT Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.DRIVER_PAYOUT))
  let baseUrl = fromMaybe "" merchantMessage.jsonData.var1
      payoutUrl = baseUrl <> "id=" <> fromMaybe "" req.bookingId
  let msg =
        merchantMessage.message
          & T.replace (templateText "numeric") req.payoutAmount
          & T.replace (templateText "url") payoutUrl
  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildGenericMessageReq = BuildGenericMessageReq {}
  deriving (Generic)

buildGenericMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> DMM.MessageKey -> Maybe DVC.VehicleCategory -> BuildGenericMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildGenericMessage merchantOpCityId messageKey vehicleCategory _ = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey vehicleCategory Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let jsonData = merchantMessage.jsonData
  let msg =
        merchantMessage.message
          & T.replace (templateText "var1") (fromMaybe "" jsonData.var1)
          & T.replace (templateText "var2") (fromMaybe "" jsonData.var2)
          & T.replace (templateText "var3") (fromMaybe "" jsonData.var3)

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

addBroadcastMessageToKafka :: (HasField "broadcastMessageTopic" r KafkaTopic, HasKafkaProducer r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Bool -> Message.RawMessage -> Id P.Person -> m ()
addBroadcastMessageToKafka check msg driverId = do
  topicName <- asks (.broadcastMessageTopic)
  msgDict <- createMessageLanguageDict msg
  produceMessage
    (if check then "broadcast-message-check" else topicName, Just (encodeUtf8 $ getId driverId))
    msgDict
  where
    createMessageLanguageDict :: (HasKafkaProducer r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Message.RawMessage -> m Message.MessageDict
    createMessageLanguageDict msg' = do
      translations <- B.runInReplica $ MTQuery.findByMessageId msg'.id
      pure $ Message.MessageDict msg' (M.fromList $ map (addTranslation msg') translations)

    addTranslation Message.RawMessage {..} trans =
      (show trans.language, Message.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

data BuildSendReceiptMessageReq = BuildSendReceiptMessageReq
  { totalFare :: Text,
    totalDistance :: Text,
    referralCode :: Text,
    rideShortId :: Text
  }

buildSendReceiptMessage :: (EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig, "meterRideReferralLink" ::: Text]) => Id DMOC.MerchantOperatingCity -> BuildSendReceiptMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildSendReceiptMessage merchantOperatingCityId req = do
  meterRideReferralLink <- asks (.meterRideReferralLink)
  let referralLink = T.replace "{referralCode}" req.referralCode meterRideReferralLink
  shortReferralLink <- UrlShortner.generateShortUrl (UrlShortner.GenerateShortUrlReq referralLink Nothing Nothing Nothing UrlShortner.METER_RIDE_REFERRAL_LINK)
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId DMM.SEND_FARE_RECEIPT_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_FARE_RECEIPT_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "rideIdAndFare") (req.rideShortId <> " " <> req.totalFare)
          & T.replace (templateText "referralLink") shortReferralLink.shortUrl

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildFleetJoiningMessageReq = BuildFleetJoiningMessageReq
  { fleetOwnerName :: Text,
    otp :: Text
  }

buildFleetJoiningMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildFleetJoiningMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildFleetJoiningMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId DMM.FLEET_JOINING_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FLEET_JOINING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName
          & T.replace (templateText "otp") req.otp

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

data BuildOperatorJoiningMessageReq = BuildOperatorJoiningMessageReq
  { operatorName :: Text,
    otp :: Text
  }

buildOperatorJoiningMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildOperatorJoiningMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildOperatorJoiningMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId DMM.OPERATOR_JOINING_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.OPERATOR_JOINING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "operatorName") req.operatorName
          & T.replace (templateText "otp") req.otp

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

newtype BuildDownloadAppMessageReq = BuildDownloadAppMessageReq
  { fleetOwnerName :: Text
  }

buildFleetJoinAndDownloadAppMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildDownloadAppMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildFleetJoinAndDownloadAppMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId DMM.FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

newtype BuildOperatorJoinAndDownloadAppMessageReq = BuildOperatorJoinAndDownloadAppMessageReq
  { operatorName :: Text
  }

buildOperatorJoinAndDownloadAppMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildOperatorJoinAndDownloadAppMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildOperatorJoinAndDownloadAppMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId DMM.OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "operatorName") req.operatorName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

newtype BuildFleetDeepLinkAuthMessage = BuildFleetDeepLinkAuthMessage
  { fleetOwnerName :: Text
  }

buildFleetDeepLinkAuthMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildFleetDeepLinkAuthMessage -> m (Maybe Text, Text, Text, Maybe Text)
buildFleetDeepLinkAuthMessage merchantOperatingCityId req = do
  (senderHeader, staticMsg, templateId, messageType) <- buildGenericMessage merchantOperatingCityId DMM.FLEET_CONSENT_DEEPLINK_MESSAGE Nothing (BuildGenericMessageReq {})
  let dynamicMsg =
        staticMsg
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName
  pure (senderHeader, dynamicMsg, templateId, messageType)

newtype BuildOperatorDeepLinkAuthMessage = BuildOperatorDeepLinkAuthMessage
  { operatorName :: Text
  }

buildOperatorDeepLinkAuthMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildOperatorDeepLinkAuthMessage -> m (Maybe Text, Text, Text, Maybe Text)
buildOperatorDeepLinkAuthMessage merchantOperatingCityId req = do
  (senderHeader, staticMsg, templateId, messageType) <- buildGenericMessage merchantOperatingCityId DMM.OPERATOR_CONSENT_DEEPLINK_MESSAGE Nothing (BuildGenericMessageReq {})
  let dynamicMsg =
        staticMsg
          & T.replace (templateText "operatorName") req.operatorName
  pure (senderHeader, dynamicMsg, templateId, messageType)

newtype BuildFleetLinkUnlinkSuccessMessageReq = BuildFleetLinkUnlinkSuccessMessageReq
  { operatorName :: Text
  }

buildFleetLinkOrUnlinkSuccessMessage ::
  (EsqDBFlow m r, CacheFlow m r) =>
  DMM.MessageKey ->
  Id DMOC.MerchantOperatingCity ->
  BuildFleetLinkUnlinkSuccessMessageReq ->
  m (Maybe Text, Text, Text, Maybe Text)
buildFleetLinkOrUnlinkSuccessMessage messageKey merchantOpCityId req = do
  unless (messageKey `elem` [DMM.FLEET_LINK_SUCCESS_MESSAGE, DMM.FLEET_UNLINK_SUCCESS_MESSAGE])
    . throwError
    . InvalidRequest
    $ "Invalid MessageKey " <> show messageKey
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let msg = merchantMessage.message & T.replace (templateText "operatorName") req.operatorName
  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId, merchantMessage.messageType)

buildFleetLinkSuccessMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildFleetLinkUnlinkSuccessMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildFleetLinkSuccessMessage = buildFleetLinkOrUnlinkSuccessMessage DMM.FLEET_LINK_SUCCESS_MESSAGE

buildFleetUnlinkSuccessMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildFleetLinkUnlinkSuccessMessageReq -> m (Maybe Text, Text, Text, Maybe Text)
buildFleetUnlinkSuccessMessage = buildFleetLinkOrUnlinkSuccessMessage DMM.FLEET_UNLINK_SUCCESS_MESSAGE
