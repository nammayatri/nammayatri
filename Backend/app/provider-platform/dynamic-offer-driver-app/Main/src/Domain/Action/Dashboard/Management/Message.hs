{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Message where

import API.Types.ProviderPlatform.Management.Message (InputType (..))
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Message as Common
import qualified AWS.S3 as S3
import Control.Monad.Extra (mapMaybeM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Text as T
import Data.Tuple.Extra (secondM)
import qualified Data.Vector as V
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Message as Domain
import qualified Domain.Types.MessageReport as Domain
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import qualified IssueManagement.Domain.Types.MediaFile as Domain
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Forkable (fork), GuidLike (generateGUID), MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, throwError)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.MessageBuilder (addBroadcastMessageToKafka)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Message as MQuery
import qualified Storage.Queries.MessageReport as MRQuery
import System.Environment (lookupEnv)
import Tools.Error

lookupBroadcastPush :: IO Bool
lookupBroadcastPush = fromMaybe False . (>>= readMaybe) <$> lookupEnv "BROADCAST_KAFKA_PUSH"

postMessageAddLink :: ShortId DM.Merchant -> Context.City -> Common.AddLinkAsMedia -> Flow Common.UploadFileResponse
postMessageAddLink merchantShortId _ req = do
  _ <- findMerchantByShortId merchantShortId
  createMediaEntry req

createMediaEntry :: Common.AddLinkAsMedia -> Flow Common.UploadFileResponse
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  _ <- MFQuery.create fileEntity
  return $ Common.UploadFileResponse {fileId = cast $ fileEntity.id}
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.MediaFile
          { id,
            _type = fileType,
            url = fileUrl,
            s3FilePath = Nothing,
            createdAt = now
          }

postMessageUploadFile :: ShortId DM.Merchant -> Context.City -> Common.UploadFileRequest -> Flow Common.UploadFileResponse
postMessageUploadFile merchantShortId opCity Common.UploadFileRequest {..} = do
  -- _ <- validateContentType
  merchant <- findMerchantByShortId merchantShortId
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- S3.createFilePath "/message-media/" ("org-" <> merchant.id.getId) fileType "" -- TODO: last param is extension (removed it as the content-type header was not comming with proxy api)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "message"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 put file" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType}

-- where
-- validateContentType = do
--   case fileType of
--     Common.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
--     Common.Image | reqContentType == "image/png" -> pure "png"
--     Common.Video | reqContentType == "video/mp4" -> pure "mp4"
--     _ -> throwError $ InternalError "UnsupportedFileFormat"

toDomainType :: Common.MessageType -> Domain.MessageType
toDomainType = \case
  Common.Read -> Domain.Read
  Common.Action str -> Domain.Action str

toDomainDeliveryStatusType :: Common.MessageDeliveryStatus -> Domain.DeliveryStatus
toDomainDeliveryStatusType = \case
  Common.Success -> Domain.Success
  Common.Queued -> Domain.Queued
  Common.Sending -> Domain.Sending
  Common.Failed -> Domain.Failed

toCommonDeliveryStatusType :: Domain.DeliveryStatus -> Common.MessageDeliveryStatus
toCommonDeliveryStatusType = \case
  Domain.Success -> Common.Success
  Domain.Queued -> Common.Queued
  Domain.Sending -> Common.Sending
  Domain.Failed -> Common.Failed

toCommonType :: Domain.MessageType -> Common.MessageType
toCommonType = \case
  Domain.Read -> Common.Read
  Domain.Action str -> Common.Action str

translationToDomainType :: UTCTime -> Common.MessageTranslation -> Domain.MessageTranslation
translationToDomainType createdAt Common.MessageTranslation {..} = Domain.MessageTranslation {..}

postMessageAdd :: ShortId DM.Merchant -> Context.City -> Common.AddMessageRequest -> Flow Common.AddMessageResponse
postMessageAdd merchantShortId opCity Common.AddMessageRequest {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  message <- mkMessage merchant merchantOpCityId
  _ <- MQuery.createMessage message
  return $ Common.AddMessageResponse {messageId = cast $ message.id}
  where
    mkMessage merchant merchantOpCityId = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.Message
          { id,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            _type = toDomainType _type,
            title,
            label,
            likeCount = 0,
            viewCount = 0,
            alwaysTriggerOnOnboarding = fromMaybe False alwaysTriggerOnOnboarding,
            description,
            shareable = False,
            shortDescription,
            mediaFiles = cast <$> mediaFiles,
            messageTranslations = translationToDomainType now <$> translations,
            createdAt = now
          }

postMessageEdit :: ShortId DM.Merchant -> Context.City -> Common.EditMessageRequest -> Flow APISuccess
postMessageEdit _ _ req@Common.EditMessageRequest {messageId} = do
  _ <- B.runInReplica $ MQuery.findById (cast messageId) >>= fromMaybeM (InvalidRequest "Message Not Found")
  result <- MQuery.updateMessage req
  case result of
    Left err -> throwError $ InvalidRequest err
    Right _ -> return Success

newtype CSVRow = CSVRow {driverId :: String}

instance FromNamedRecord CSVRow where
  parseNamedRecord r = CSVRow <$> r .: "driverId"

postMessageSend :: ShortId DM.Merchant -> Context.City -> Common.SendMessageRequest -> Flow APISuccess
postMessageSend merchantShortId opCity Common.SendMessageRequest {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  message <- B.runInReplica $ MQuery.findById (Id messageId) >>= fromMaybeM (InvalidRequest "Message Not Found")
  kafkaPush <- L.runIO lookupBroadcastPush
  allDriverIds <- case _type of
    AllEnabled -> B.runInReplica $ QDI.findAllDriverIdExceptProvided merchant merchantOpCity []
    Include -> readCsv
    Exclude -> do
      driverIds <- readCsv
      B.runInReplica $ QDI.findAllDriverIdExceptProvided merchant merchantOpCity driverIds
  logDebug $ "DriverId to which the message is sent" <> show allDriverIds
  now <- getCurrentTime
  MQuery.updateShareable (_type == AllEnabled || message.shareable) message.id
  case scheduledTime of
    Just scheduleTime
      | now <= scheduleTime ->
        QAllJ.createJobByTime @_ @'ScheduledFCMS (Just merchant.id) (Just merchantOpCity.id) scheduleTime $
          ScheduledFCMSJobData allDriverIds message
      | otherwise -> throwError (InvalidRequest "Scheduled Time cannot be in the past")
    Nothing -> fork "Adding messages to Kafka queue" $ mapM_ (addBroadcastMessageToKafka kafkaPush message) allDriverIds

  return Success
  where
    readCsv = case csvFile of
      Just csvFile_ -> do
        csvData <- L.runIO $ BS.readFile csvFile_
        case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CSVRow)) of
          Left err -> throwError (InvalidRequest $ show err)
          Right (_, v) -> pure $ V.toList $ V.map (Id . T.pack . (.driverId)) v
      Nothing -> throwError (InvalidRequest "CSV FilePath Not Found")

getMessageList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.MessageListResponse
getMessageList merchantShortId opCity mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  messages <- MQuery.findAllWithLimitOffset mbLimit mbOffset merchant merchantOpCity
  let count = length messages
  let summary = Common.Summary {totalCount = count, count}
  return $ Common.MessageListResponse {messages = buildMessage <$> messages, summary}
  where
    buildMessage :: Domain.RawMessage -> Common.MessageListItem
    buildMessage message = do
      Common.MessageListItem
        { messageId = cast message.id,
          title = message.title,
          _type = toCommonType message._type,
          shareable = message.shareable
        }

getMessageInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Message -> Flow Common.MessageInfoResponse
getMessageInfo merchantShortId _ reqMessageId = do
  let messageId = cast reqMessageId
  _ <- findMerchantByShortId merchantShortId
  message <- MQuery.findById messageId >>= fromMaybeM (InvalidRequest "Message Not Found")
  mediaFiles <- mapMaybeM MFQuery.findById message.mediaFiles
  return $ buildMessageInfoResponse mediaFiles message
  where
    buildMessageInfoResponse mf Domain.RawMessage {..} =
      Common.MessageInfoResponse
        { messageId = cast id,
          _type = toCommonType _type,
          description,
          shortDescription,
          title,
          shareable = shareable,
          mediaFiles = (\mediaFile -> Common.MediaFile mediaFile._type mediaFile.url) <$> mf
        }

getMessageDeliveryInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Message -> Flow Common.MessageDeliveryInfoResponse
getMessageDeliveryInfo merchantShortId _ reqMessageId = do
  let messageId = cast reqMessageId
  _ <- findMerchantByShortId merchantShortId
  success <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Success
  failed <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Failed
  queued <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Queued
  sending <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Sending
  seen <- B.runInReplica $ MRQuery.getMessageCountByReadStatus messageId
  message <- MQuery.findById messageId >>= fromMaybeM (InvalidRequest "Message Not Found")
  return $ Common.MessageDeliveryInfoResponse {messageId = cast messageId, success, failed, queued, sending, seen, liked = message.likeCount, viewed = message.viewCount}

getMessageReceiverList :: ShortId DM.Merchant -> Context.City -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Flow Common.MessageReceiverListResponse
getMessageReceiverList merchantShortId _ reqMsgId _ mbStatus mbLimit mbOffset = do
  let msgId = cast reqMsgId
  _ <- findMerchantByShortId merchantShortId
  encMesageReports <- B.runInReplica $ MRQuery.findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset msgId $ toDomainDeliveryStatusType <$> mbStatus
  messageReports <- mapM (secondM decrypt) encMesageReports
  let count = length messageReports
  let summary = Common.Summary {totalCount = count, count}

  return $ Common.MessageReceiverListResponse {receivers = buildReceiverListItem <$> messageReports, summary}
  where
    buildReceiverListItem (Domain.MessageReport {..}, person) = do
      Common.MessageReceiverListItem
        { receiverId = cast driverId,
          receiverName = person.firstName,
          receiverNumber = fromMaybe "" person.mobileNumber,
          reply,
          seen = Just readStatus,
          liked = Just likeStatus,
          status = toCommonDeliveryStatusType deliveryStatus
        }
