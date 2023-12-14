{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Message where

import qualified AWS.S3 as S3
import Control.Monad.Extra (mapMaybeM)
import Dashboard.Common.Message (InputType (..))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Tuple.Extra (secondM)
import qualified Data.Vector as V
import qualified Domain.Types.Merchant as DM
import Domain.Types.Message.Message as MS
import qualified Domain.Types.Message.Message as Domain
import qualified Domain.Types.Message.MessageReport as Domain
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import qualified IssueManagement.Domain.Types.MediaFile as Domain
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Forkable (fork), GuidLike (generateGUID), MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.Message.Message as MQuery
import qualified Storage.Queries.Message.MessageReport as MRQuery
import qualified Storage.Queries.Message.MessageTranslation as MTQuery
import qualified Storage.Queries.Person as QP
import Tools.Error

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

addLinkAsMedia :: ShortId DM.Merchant -> Context.City -> Common.AddLinkAsMedia -> Flow Common.UploadFileResponse
addLinkAsMedia merchantShortId _ req = do
  _ <- findMerchantByShortId merchantShortId
  createMediaEntry req

createMediaEntry :: Common.AddLinkAsMedia -> Flow Common.UploadFileResponse
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  _ <- MFQuery.create fileEntity
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

uploadFile :: ShortId DM.Merchant -> Context.City -> Common.UploadFileRequest -> Flow Common.UploadFileResponse
uploadFile merchantShortId opCity Common.UploadFileRequest {..} = do
  -- _ <- validateContentType
  merchant <- findMerchantByShortId merchantShortId
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath merchant.id.getId fileType "" -- TODO: last param is extension (removed it as the content-type header was not comming with proxy api)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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

addMessage :: ShortId DM.Merchant -> Context.City -> Common.AddMessageRequest -> Flow Common.AddMessageResponse
addMessage merchantShortId _ Common.AddMessageRequest {..} = do
  merchant <- findMerchantByShortId merchantShortId
  message <- mkMessage merchant
  _ <- MQuery.create message
  return $ Common.AddMessageResponse {messageId = cast $ message.id}
  where
    mkMessage merchant = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.Message
          { id,
            merchantId = merchant.id,
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

newtype CSVRow = CSVRow {driverId :: String}

instance FromNamedRecord CSVRow where
  parseNamedRecord r = CSVRow <$> r .: "driverId"

sendMessage :: ShortId DM.Merchant -> Context.City -> Common.SendMessageRequest -> Flow APISuccess
sendMessage merchantShortId _ Common.SendMessageRequest {..} = do
  now <- getCurrentTime
  merchant <- findMerchantByShortId merchantShortId
  message <- B.runInReplica $ MQuery.findById (Id messageId) >>= fromMaybeM (InvalidRequest "Message Not Found")
  MRQuery.updateSentAtByMessageIds [getField @"id" message] now
  -- let updatedMessage = message & MS.sentAt .~ (Just now)
  let updatedMessage = message {MS.sentAt = Just now}
  allDriverIds <- case _type of
    AllEnabled -> B.runInReplica $ QP.findAllDriverIdExceptProvided (merchant.id) []
    Include -> readCsv
    Exclude -> do
      driverIds <- readCsv
      B.runInReplica $ QP.findAllDriverIdExceptProvided (merchant.id) driverIds
  logDebug $ "DriverId to which the message is sent" <> show allDriverIds
  fork "Adding messages to kafka queue" $ mapM_ (addToKafka updatedMessage) allDriverIds
  return Success
  where
    addToKafka message driverId = do
      topicName <- asks (.broadcastMessageTopic)
      msg <- createMessageLanguageDict message
      produceMessage
        (topicName, Just (encodeUtf8 $ getId driverId))
        msg

    readCsv = case csvFile of
      Just csvFile_ -> do
        csvData <- L.runIO $ BS.readFile csvFile_
        case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CSVRow)) of
          Left err -> throwError (InvalidRequest $ show err)
          Right (_, v) -> pure $ V.toList $ V.map (Id . T.pack . (.driverId)) v
      Nothing -> throwError (InvalidRequest "CSV FilePath Not Found")

    createMessageLanguageDict :: Domain.RawMessage -> Flow Domain.MessageDict
    createMessageLanguageDict message = do
      translations <- B.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

    addTranslation Domain.RawMessage {..} trans =
      (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

messageList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.MessageListResponse
messageList merchantShortId _ mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  messages <- MQuery.findAllWithLimitOffset mbLimit mbOffset merchant.id
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

messageInfo :: ShortId DM.Merchant -> Context.City -> Id Domain.Message -> Flow Common.MessageInfoResponse
messageInfo merchantShortId _ messageId = do
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
          mediaFiles = (\mediaFile -> Common.MediaFile (toCommonMediaFileType mediaFile._type) mediaFile.url) <$> mf
        }

messageDeliveryInfo :: ShortId DM.Merchant -> Context.City -> Id Domain.Message -> Flow Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId _ messageId = do
  _ <- findMerchantByShortId merchantShortId
  success <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Success
  failed <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Failed
  queued <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Queued
  sending <- B.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Sending
  seen <- B.runInReplica $ MRQuery.getMessageCountByReadStatus messageId
  message <- MQuery.findById messageId >>= fromMaybeM (InvalidRequest "Message Not Found")
  return $ Common.MessageDeliveryInfoResponse {messageId = cast messageId, success, failed, queued, sending, seen, liked = message.likeCount, viewed = message.viewCount}

messageReceiverList :: ShortId DM.Merchant -> Context.City -> Id Domain.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Flow Common.MessageReceiverListResponse
messageReceiverList merchantShortId _ msgId _ mbStatus mbLimit mbOffset = do
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
