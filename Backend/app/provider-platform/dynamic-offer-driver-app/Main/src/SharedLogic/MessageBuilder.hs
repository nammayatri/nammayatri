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
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Streaming.Kafka.Commons
import Kernel.Streaming.Kafka.Producer
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.Queries.MessageTranslation as MTQuery
import Tools.Error
import qualified UrlShortner.Common as UrlShortner

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

-- Helper function to find merchant message with language fallback
findMerchantMessageWithLanguageFallback :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> DMM.MessageKey -> Maybe DVC.VehicleCategory -> Maybe Language -> Maybe [LYT.ConfigVersionMap] -> m (Maybe DMM.MerchantMessage)
findMerchantMessageWithLanguageFallback merchantOpCityId messageKey vehicleCategory mbLanguage mbConfigVersionMap =
  case mbLanguage of
    Just language ->
      QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategoryAndLanguage merchantOpCityId messageKey vehicleCategory (Just language) mbConfigVersionMap
    Nothing ->
      QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey vehicleCategory mbConfigVersionMap

data BuildSendPaymentLinkReq = BuildSendPaymentLinkReq
  { paymentLink :: Text,
    amount :: Text
  }
  deriving (Generic)

buildSendPaymentLink :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildSendPaymentLinkReq -> m (Maybe Text, Text, Text)
buildSendPaymentLink merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.SEND_PAYMENT_LINK Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.SEND_PAYMENT_LINK))
  let msg =
        merchantMessage.message
          & T.replace (templateText "paymentLink") req.paymentLink
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildSendOTPMessageReq = BuildSendOTPMessageReq
  { otp :: Text,
    hash :: Text
  }
  deriving (Generic)

buildSendOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildSendOTPMessageReq -> m (Maybe Text, Text, Text)
buildSendOTPMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.SEND_OTP Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.SEND_OTP))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "hash") req.hash

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

newtype WelcomeToPlatformMessageReq = WelcomeToPlatformMessageReq
  { orgName :: Text
  }
  deriving (Generic)

buildWelcomeToPlatformMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> WelcomeToPlatformMessageReq -> m (Maybe Text, Text, Text)
buildWelcomeToPlatformMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.WELCOME_TO_PLATFORM Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.WELCOME_TO_PLATFORM))
  let msg =
        merchantMessage.message
          & T.replace (templateText "orgName") req.orgName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

buildSendAlternateNumberOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildSendOTPMessageReq -> m (Maybe Text, Text, Text)
buildSendAlternateNumberOTPMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.ALTERNATE_NUMBER_OTP Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.ALTERNATE_NUMBER_OTP))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "hash") req.hash

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildEndRideMessageReq = BuildEndRideMessageReq
  { rideAmount :: Text,
    rideShortId :: Text
  }
  deriving (Generic)

buildEndRideMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildEndRideMessageReq -> m (Maybe Text, Text, Text)
buildEndRideMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.END_RIDE_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.END_RIDE_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "rideAmount") req.rideAmount
          & T.replace (templateText "rideId") req.rideShortId

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildOnboardingMessageReq = BuildOnboardingMessageReq {}
  deriving (Generic)

buildOnboardingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildOnboardingMessageReq -> m (Maybe Text, Text, Text)
buildOnboardingMessage merchantOpCityId mbLanguage _ = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.ONBOARDING_YATRI_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.ONBOARDING_YATRI_MESSAGE))

  pure (merchantMessage.senderHeader, merchantMessage.message, merchantMessage.templateId)

data BuildBookingMessageReq = BuildBookingMessageReq
  { otp :: Text,
    amount :: Text
  }
  deriving (Generic)

buildBookingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildBookingMessageReq -> m (Maybe Text, Text, Text)
buildBookingMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.BOOKING_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.BOOKING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "otp") req.otp
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

newtype BuildCollectCashMessageReq = BuildCollectCashMessageReq
  { amount :: Text
  }
  deriving (Generic)

buildCollectCashMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildCollectCashMessageReq -> m (Maybe Text, Text, Text)
buildCollectCashMessage merchantOpCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId DMM.CASH_COLLECTED_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show DMM.CASH_COLLECTED_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "amount") req.amount

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildGenericMessageReq = BuildGenericMessageReq {}
  deriving (Generic)

buildGenericMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> DMM.MessageKey -> Maybe DVC.VehicleCategory -> BuildGenericMessageReq -> m (Maybe Text, Text, Text)
buildGenericMessage merchantOpCityId mbLanguage messageKey vehicleCategory _ = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId messageKey vehicleCategory mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let jsonData = merchantMessage.jsonData
  let msg =
        merchantMessage.message
          & T.replace (templateText "var1") (fromMaybe "" jsonData.var1)
          & T.replace (templateText "var2") (fromMaybe "" jsonData.var2)
          & T.replace (templateText "var3") (fromMaybe "" jsonData.var3)

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

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

buildSendReceiptMessage :: (EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig, "meterRideReferralLink" ::: Text]) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildSendReceiptMessageReq -> m (Maybe Text, Text, Text)
buildSendReceiptMessage merchantOperatingCityId mbLanguage req = do
  meterRideReferralLink <- asks (.meterRideReferralLink)
  let referralLink = T.replace "{referralCode}" req.referralCode meterRideReferralLink
  shortReferralLink <- UrlShortner.generateShortUrl (UrlShortner.GenerateShortUrlReq referralLink Nothing Nothing Nothing UrlShortner.METER_RIDE_REFERRAL_LINK)
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOperatingCityId DMM.SEND_FARE_RECEIPT_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_FARE_RECEIPT_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "rideIdAndFare") (req.rideShortId <> " " <> req.totalFare)
          & T.replace (templateText "referralLink") shortReferralLink.shortUrl

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildFleetJoiningMessageReq = BuildFleetJoiningMessageReq
  { fleetOwnerName :: Text,
    otp :: Text
  }

buildFleetJoiningMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildFleetJoiningMessageReq -> m (Maybe Text, Text, Text)
buildFleetJoiningMessage merchantOperatingCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOperatingCityId DMM.FLEET_JOINING_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FLEET_JOINING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName
          & T.replace (templateText "otp") req.otp

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

data BuildOperatorJoiningMessageReq = BuildOperatorJoiningMessageReq
  { operatorName :: Text,
    otp :: Text
  }

buildOperatorJoiningMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildOperatorJoiningMessageReq -> m (Maybe Text, Text, Text)
buildOperatorJoiningMessage merchantOperatingCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOperatingCityId DMM.OPERATOR_JOINING_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.OPERATOR_JOINING_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "operatorName") req.operatorName
          & T.replace (templateText "otp") req.otp

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

newtype BuildDownloadAppMessageReq = BuildDownloadAppMessageReq
  { fleetOwnerName :: Text
  }

buildFleetJoinAndDownloadAppMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildDownloadAppMessageReq -> m (Maybe Text, Text, Text)
buildFleetJoinAndDownloadAppMessage merchantOperatingCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOperatingCityId DMM.FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

newtype BuildOperatorJoinAndDownloadAppMessageReq = BuildOperatorJoinAndDownloadAppMessageReq
  { operatorName :: Text
  }

buildOperatorJoinAndDownloadAppMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildOperatorJoinAndDownloadAppMessageReq -> m (Maybe Text, Text, Text)
buildOperatorJoinAndDownloadAppMessage merchantOperatingCityId mbLanguage req = do
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOperatingCityId DMM.OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE))
  let msg =
        merchantMessage.message
          & T.replace (templateText "operatorName") req.operatorName

  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

newtype BuildFleetDeepLinkAuthMessage = BuildFleetDeepLinkAuthMessage
  { fleetOwnerName :: Text
  }

buildFleetDeepLinkAuthMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildFleetDeepLinkAuthMessage -> m (Maybe Text, Text, Text)
buildFleetDeepLinkAuthMessage merchantOperatingCityId mbLanguage req = do
  (senderHeader, staticMsg, templateId) <- buildGenericMessage merchantOperatingCityId mbLanguage DMM.FLEET_CONSENT_DEEPLINK_MESSAGE Nothing (BuildGenericMessageReq {})
  let dynamicMsg =
        staticMsg
          & T.replace (templateText "fleetOwnerName") req.fleetOwnerName
  pure (senderHeader, dynamicMsg, templateId)

newtype BuildOperatorDeepLinkAuthMessage = BuildOperatorDeepLinkAuthMessage
  { operatorName :: Text
  }

buildOperatorDeepLinkAuthMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildOperatorDeepLinkAuthMessage -> m (Maybe Text, Text, Text)
buildOperatorDeepLinkAuthMessage merchantOperatingCityId mbLanguage req = do
  (senderHeader, staticMsg, templateId) <- buildGenericMessage merchantOperatingCityId mbLanguage DMM.OPERATOR_CONSENT_DEEPLINK_MESSAGE Nothing (BuildGenericMessageReq {})
  let dynamicMsg =
        staticMsg
          & T.replace (templateText "operatorName") req.operatorName
  pure (senderHeader, dynamicMsg, templateId)

newtype BuildFleetLinkUnlinkSuccessMessageReq = BuildFleetLinkUnlinkSuccessMessageReq
  { operatorName :: Text
  }

buildFleetLinkOrUnlinkSuccessMessage ::
  (EsqDBFlow m r, CacheFlow m r) =>
  DMM.MessageKey ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Language ->
  BuildFleetLinkUnlinkSuccessMessageReq ->
  m (Maybe Text, Text, Text)
buildFleetLinkOrUnlinkSuccessMessage messageKey merchantOpCityId mbLanguage req = do
  unless (messageKey `elem` [DMM.FLEET_LINK_SUCCESS_MESSAGE, DMM.FLEET_UNLINK_SUCCESS_MESSAGE])
    . throwError
    . InvalidRequest
    $ "Invalid MessageKey " <> show messageKey
  merchantMessage <-
    findMerchantMessageWithLanguageFallback merchantOpCityId messageKey Nothing mbLanguage Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let msg = merchantMessage.message & T.replace (templateText "operatorName") req.operatorName
  pure (merchantMessage.senderHeader, msg, merchantMessage.templateId)

buildFleetLinkSuccessMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildFleetLinkUnlinkSuccessMessageReq -> m (Maybe Text, Text, Text)
buildFleetLinkSuccessMessage merchantOpCityId mbLanguage = buildFleetLinkOrUnlinkSuccessMessage DMM.FLEET_LINK_SUCCESS_MESSAGE merchantOpCityId mbLanguage

buildFleetUnlinkSuccessMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Language -> BuildFleetLinkUnlinkSuccessMessageReq -> m (Maybe Text, Text, Text)
buildFleetUnlinkSuccessMessage merchantOpCityId mbLanguage = buildFleetLinkOrUnlinkSuccessMessage DMM.FLEET_UNLINK_SUCCESS_MESSAGE merchantOpCityId mbLanguage
