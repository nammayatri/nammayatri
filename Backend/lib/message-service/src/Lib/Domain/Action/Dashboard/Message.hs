{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Lib.Domain.Action.Dashboard.Message where

import qualified AWS.S3 as S3
import Control.Monad.Extra (mapMaybeM)
import Dashboard.Common.Message (InputType (..))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.HashMap as HM
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Domain.Types.MediaFile as Domain
import qualified Lib.Domain.Types.Message.Message as Domain
import qualified Lib.Domain.Types.Message.MessageReport as Domain
import qualified Lib.Storage.Queries.MediaFile as MFQuery
import qualified Lib.Storage.Queries.Message.Message as MQuery
import qualified Lib.Storage.Queries.Message.MessageReport as MRQuery
import qualified Lib.Storage.Queries.Message.MessageTranslation as MTQuery

data TransporterConfig

data ServiceHandle m = ServiceHandle
  { findAllPersonIdExceptProvided :: Id Domain.Merchant -> [Id Domain.Person] -> m [Id Domain.Person],
    findPersonInIds :: [Id Domain.Person] -> m [Domain.Person]
  }

createFilePath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  Common.FileType ->
  Text ->
  m Text
createFilePath merchantId fileType validatedFileExtention = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/message-media/" <> "org-" <> merchantId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> validatedFileExtention
    )

addLinkAsMedia :: (MonadTime m, MonadGuid m, MonadReader r m, MonadIO m, Esq.EsqDBFlow m r) => Common.AddLinkAsMedia -> m Common.UploadFileResponse
addLinkAsMedia req = do
  createMediaEntry req

createMediaEntry :: (MonadTime m, MonadGuid m, MonadReader r m, MonadIO m, Esq.EsqDBFlow m r) => Common.AddLinkAsMedia -> m Common.UploadFileResponse
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  Esq.runTransaction $ MFQuery.create fileEntity
  return $ Common.UploadFileResponse {fileId = cast $ fileEntity.id}
  where
    mapToDomain = \case
      Common.Audio -> Domain.Audio
      Common.Video -> Domain.Video
      Common.Image -> Domain.Image
      Common.AudioLink -> Domain.AudioLink
      Common.VideoLink -> Domain.VideoLink
      Common.ImageLink -> Domain.ImageLink
      Common.PortraitVideoLink -> Domain.PortraitVideoLink

    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.MediaFile
          { id,
            _type = mapToDomain fileType,
            url = fileUrl,
            createdAt = now
          }

uploadFile :: (MonadReader r m, MonadIO m, MonadGuid m, Forkable m, MonadThrow m, Esq.EsqDBFlow m r, MonadTime m, Log m, L.MonadFlow m, HasField "s3Env" r (S3.S3Env m), HasField "mediaFileUrlPattern" transporterConfig Text) => Common.UploadFileRequest -> Id Domain.Merchant -> Maybe transporterConfig -> m Common.UploadFileResponse
uploadFile Common.UploadFileRequest {..} merchantId transConfig = do
  -- _ <- validateContentType
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath merchantId.getId fileType "" -- TODO: last param is extension (removed it as the content-type header was not comming with proxy api)
  transporterConfig <- fromMaybeM (TransporterConfigNotFound merchantId.getId) transConfig
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

toCommonMediaFileType :: Domain.MediaType -> Common.FileType
toCommonMediaFileType = \case
  Domain.Audio -> Common.Audio
  Domain.Video -> Common.Video
  Domain.Image -> Common.Image
  Domain.ImageLink -> Common.ImageLink
  Domain.VideoLink -> Common.VideoLink
  Domain.AudioLink -> Common.AudioLink
  Domain.PortraitVideoLink -> Common.PortraitVideoLink

translationToDomainType :: UTCTime -> Common.MessageTranslation -> Domain.MessageTranslation
translationToDomainType createdAt Common.MessageTranslation {..} = Domain.MessageTranslation {..}

addMessage ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    MonadIO m,
    Esq.EsqDBReplicaFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Common.AddMessageRequest ->
  Id Domain.Merchant ->
  m Common.AddMessageResponse
addMessage Common.AddMessageRequest {..} merchantId = do
  message <- mkMessage
  Esq.runTransaction $ MQuery.create message
  return $ Common.AddMessageResponse {messageId = cast $ message.id}
  where
    mkMessage = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.Message
          { id,
            merchantId = merchantId,
            _type = toDomainType _type,
            title,
            label,
            likeCount = 0,
            viewCount = 0,
            description,
            shortDescription,
            mediaFiles = cast <$> mediaFiles,
            messageTranslations = translationToDomainType now <$> translations,
            createdAt = now
          }

newtype CSVRow = CSVRow {personId :: String}

instance FromNamedRecord CSVRow where
  parseNamedRecord r = CSVRow <$> r .: "personId"

sendMessage :: (Applicative m, MonadThrow m, Log m, L.MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], Esq.EsqDBReplicaFlow m r, Esq.EsqDBFlow m r, HasField "broadcastMessageTopic" r KafkaTopic) => Common.SendMessageRequest -> Id Domain.Merchant -> ServiceHandle m -> m APISuccess
sendMessage Common.SendMessageRequest {..} mId sendMessageHandle = do
  message <- Esq.runInReplica $ MQuery.findById (Id messageId) >>= fromMaybeM (InvalidRequest "Message Not Found")
  allPersonIds <- case _type of
    AllEnabled -> sendMessageHandle.findAllPersonIdExceptProvided mId []
    Include -> readCsv
    Exclude -> do
      personIds <- readCsv
      sendMessageHandle.findAllPersonIdExceptProvided mId personIds
  logDebug $ "PersonId to which the message is sent" <> show allPersonIds
  fork "Adding messages to kafka queue" $ mapM_ (addToKafka message) allPersonIds
  return Success
  where
    addToKafka message personId = do
      topicName <- asks (.broadcastMessageTopic)
      now <- getCurrentTime
      void $ try @_ @SomeException (Esq.runTransaction $ MRQuery.create (mkMessageReport now personId)) -- avoid extra DB call to check if personId exists
      msg <- createMessageLanguageDict message
      produceMessage
        (topicName, Just (encodeUtf8 $ getId personId))
        msg

    readCsv = case csvFile of
      Just csvFile_ -> do
        csvData <- L.runIO $ BS.readFile csvFile_
        case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CSVRow)) of
          Left err -> throwError (InvalidRequest $ show err)
          Right (_, v) -> pure $ V.toList $ V.map (Id . T.pack . (.personId)) v
      Nothing -> throwError (InvalidRequest "CSV FilePath Not Found")

    createMessageLanguageDict :: (Applicative m, MonadClock m, MonadGuid m, Log m, Forkable m, L.MonadFlow m, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => Domain.RawMessage -> m Domain.MessageDict
    createMessageLanguageDict message = do
      translations <- Esq.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

    addTranslation Domain.RawMessage {..} trans =
      (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

    mkMessageReport now personId =
      Domain.MessageReport
        { personId,
          messageId = Id messageId,
          deliveryStatus = Domain.Queued,
          readStatus = False,
          likeStatus = False,
          messageDynamicFields = M.empty,
          reply = Nothing,
          createdAt = now,
          updatedAt = now
        }

messageList ::
  ( MonadTime m,
    Monad m,
    MonadReader r m,
    MonadIO m,
    MonadThrow m,
    Log m,
    Esq.EsqDBFlow m r
  ) =>
  Maybe Int ->
  Maybe Int ->
  Id Domain.Merchant ->
  m Common.MessageListResponse
messageList mbLimit mbOffset merchantId = do
  messages <- MQuery.findAllWithLimitOffset mbLimit mbOffset merchantId
  let count = length messages
  let summary = Common.Summary {totalCount = count, count}
  return $ Common.MessageListResponse {messages = buildMessage <$> messages, summary}
  where
    buildMessage :: Domain.RawMessage -> Common.MessageListItem
    buildMessage message = do
      Common.MessageListItem
        { messageId = cast message.id,
          title = message.title,
          _type = toCommonType message._type
        }

messageInfo ::
  ( MonadTime m,
    Monad m,
    MonadReader r m,
    MonadIO m,
    Esq.EsqDBReplicaFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Id Domain.Message ->
  m Common.MessageInfoResponse
messageInfo messageId = do
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
          mediaFiles = (\mediaFile -> Common.MediaFile (toCommonMediaFileType mediaFile._type) mediaFile.url) <$> mf
        }

messageDeliveryInfo ::
  ( Monad m,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadClock m
  ) =>
  Id Domain.Message ->
  m Common.MessageDeliveryInfoResponse
messageDeliveryInfo messageId = do
  success <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Success
  failed <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Failed
  queued <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Queued
  sending <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Sending
  seen <- Esq.runInReplica $ MRQuery.getMessageCountByReadStatus messageId
  message <- MQuery.findById messageId >>= fromMaybeM (InvalidRequest "Message Not Found")
  return $ Common.MessageDeliveryInfoResponse {messageId = cast messageId, success, failed, queued, sending, seen, liked = message.likeCount, viewed = message.viewCount}

messageReceiverList ::
  ( Monad m,
    Functor m,
    Esq.EsqDBReplicaFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  ServiceHandle m ->
  Id Domain.Message ->
  Maybe Common.MessageDeliveryStatus ->
  Maybe Int ->
  Maybe Int ->
  m Common.MessageReceiverListResponse
messageReceiverList ServiceHandle {..} msgId mbStatus mbLimit mbOffset = do
  messageReports <- Esq.runInReplica $ MRQuery.findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset msgId (toDomainDeliveryStatusType <$> mbStatus)
  let personIds = map (.personId) messageReports
  let messageReportsMap = HM.fromList $ map (\mr -> (mr.personId.getId, mr)) messageReports
  personsMap <- HM.fromList . map (\d -> (d.id.getId, d)) <$> findPersonInIds personIds
  let count = length messageReports
  let summary = Common.Summary {totalCount = count, count}
  return $ Common.MessageReceiverListResponse {receivers = HM.elems $ HM.mapWithKey (buildReceiverListItem personsMap) messageReportsMap, summary}
  where
    buildReceiverListItem personsMap pid Domain.MessageReport {..} = do
      let mbPerson = HM.lookup pid personsMap
      Common.MessageReceiverListItem
        { receiverId = Id pid,
          receiverName = maybe "" (.firstName) mbPerson,
          receiverNumber = fromMaybe "" $ (.mobileNumber) =<< mbPerson,
          reply,
          seen = Just readStatus,
          liked = Just likeStatus,
          status = toCommonDeliveryStatusType deliveryStatus
        }
