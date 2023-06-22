{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Domain.Action.UI.Message where

import qualified AWS.S3 as S3
import Data.OpenApi hiding (description, info, title, url)
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Kernel.External.Types
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import qualified Lib.Domain.Types.MediaFile as MF
import qualified Lib.Domain.Types.Message.Message as Domain
import qualified Lib.Domain.Types.Message.MessageReport as Domain
import qualified Lib.Storage.Queries.MediaFile as MFQ
import Lib.Storage.Queries.Message.Message as MQ
import qualified Lib.Storage.Queries.Message.MessageReport as MRQ

data MediaFileApiResponse = MediaFileApiResponse
  { url :: Text,
    fileType :: MF.MediaType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data MessageAPIEntityResponse = MessageAPIEntityResponse
  { title :: Text,
    description :: Text,
    _type :: Domain.MessageType,
    created_at :: UTCTime,
    label :: Maybe Text,
    reply :: Maybe Text,
    readStatus :: Bool,
    likeStatus :: Bool,
    likeCount :: Int,
    viewCount :: Int,
    messageId :: Id Domain.Message,
    mediaFiles :: [MediaFileApiResponse]
  }
  deriving (Generic, ToSchema, FromJSON)

instance ToJSON MessageAPIEntityResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype MessageReplyReq = MessageReplyReq {reply :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

messageList :: (MonadFlow m, EsqDBReplicaFlow m r, EsqDBFlow m r) => (Id Domain.Person, Id Domain.Merchant) -> Maybe Int -> Maybe Int -> Maybe Language -> m [MessageAPIEntityResponse]
messageList (personId, _) mbLimit mbOffset personLanguage = do
  messageDetails <- Esq.runInReplica $ MRQ.findByPersonIdAndLanguage (cast personId) (fromMaybe ENGLISH personLanguage) mbLimit mbOffset
  mapM makeMessageAPIEntity messageDetails
  where
    makeMessageAPIEntity (messageReport, rawMessage, messageTranslation) = do
      mediaFilesApiType <- map (\mediaFile -> MediaFileApiResponse mediaFile.url mediaFile._type) <$> MFQ.findAllIn rawMessage.mediaFiles
      pure $
        MessageAPIEntityResponse
          { title = maybe rawMessage.title (.title) messageTranslation,
            description = maybe rawMessage.description (.description) messageTranslation,
            _type = rawMessage._type,
            label = messageTranslation >>= (.label),
            reply = messageReport.reply,
            created_at = rawMessage.createdAt,
            readStatus = messageReport.readStatus,
            likeStatus = messageReport.likeStatus,
            likeCount = rawMessage.likeCount,
            viewCount = rawMessage.viewCount,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

getMessage :: (MonadFlow m, EsqDBReplicaFlow m r, EsqDBFlow m r) => (Id Domain.Person, Id Domain.Merchant) -> Id Domain.Message -> Maybe Language -> m MessageAPIEntityResponse
getMessage (personId, _) messageId personLanguage = do
  messageDetails <-
    Esq.runInReplica $
      MRQ.findByPersonIdMessageIdAndLanguage (cast personId) messageId (fromMaybe ENGLISH personLanguage)
        >>= fromMaybeM (InvalidRequest "Message not found")
  makeMessageAPIEntity messageDetails
  where
    makeMessageAPIEntity (messageReport, rawMessage, messageTranslation) = do
      mediaFilesApiType <- map (\mediaFile -> MediaFileApiResponse mediaFile.url mediaFile._type) <$> MFQ.findAllIn rawMessage.mediaFiles
      pure $
        MessageAPIEntityResponse
          { title = maybe rawMessage.title (.title) messageTranslation,
            description = maybe rawMessage.description (.description) messageTranslation,
            _type = rawMessage._type,
            label = messageTranslation >>= (.label),
            reply = messageReport.reply,
            created_at = rawMessage.createdAt,
            readStatus = messageReport.readStatus,
            likeStatus = messageReport.likeStatus,
            likeCount = rawMessage.likeCount,
            viewCount = rawMessage.viewCount,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

fetchMedia :: (MonadFlow m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) => (Id Domain.Person, Id Domain.Merchant) -> Text -> m Text
fetchMedia (_, _) filePath = do
  S3.get $ T.unpack filePath

messageSeen :: (MonadFlow m, MonadReader r m, EsqDBReplicaFlow m r, HasField "esqDBEnv" r EsqDBEnv, HasField "loggerEnv" r LoggerEnv) => (Id Domain.Person, Id Domain.Merchant) -> Id Domain.Message -> m APISuccess
messageSeen (personId, _) messageId = do
  messageDetails <- Esq.runInReplica $ MRQ.findByMessageIdAndPersonId messageId (cast personId) >>= fromMaybeM (InvalidRequest "Message not found")
  Esq.runTransaction $ do
    when (not messageDetails.readStatus) $ MQ.updateMessageViewCount messageId 1
    MRQ.updateSeenAndReplyByMessageIdAndPersonId messageId (cast personId) True Nothing
  return Success

messageLiked :: (MonadFlow m, EsqDBReplicaFlow m r, EsqDBFlow m r) => (Id Domain.Person, Id Domain.Merchant) -> Id Domain.Message -> m APISuccess
messageLiked (personId, _) messageId = do
  messageDetails <- Esq.runInReplica $ MRQ.findByMessageIdAndPersonId messageId (cast personId) >>= fromMaybeM (InvalidRequest "Message not found")
  unless (messageDetails.readStatus) $
    throwError $ InvalidRequest "Message is not seen"
  let val = if messageDetails.likeStatus then (-1) else 1
  Esq.runTransaction $ do
    when messageDetails.readStatus $ MQ.updateMessageLikeCount messageId val
    MRQ.updateMessageLikeByMessageIdAndPersonIdAndReadStatus messageId (cast personId)
  return Success

messageResponse :: (MonadFlow m, MonadReader r m, HasField "esqDBEnv" r EsqDBEnv, HasField "loggerEnv" r LoggerEnv) => (Id Domain.Person, Id Domain.Merchant) -> Id Domain.Message -> MessageReplyReq -> m APISuccess
messageResponse (personId, _) messageId MessageReplyReq {..} = do
  Esq.runTransaction $ MRQ.updateSeenAndReplyByMessageIdAndPersonId messageId (cast personId) True (Just reply)
  return Success
