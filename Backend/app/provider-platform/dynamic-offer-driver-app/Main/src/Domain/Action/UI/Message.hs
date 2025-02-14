{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Message where

import qualified AWS.S3 as S3
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Message as Domain
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Storage.Queries.MediaFile as MFQ
import qualified Kernel.Beam.Functions as B
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Message as MQ
import qualified Storage.Queries.MessageReport as MRQ
import qualified Storage.Queries.Person as QP
import Tools.Error

data MediaFileApiResponse = MediaFileApiResponse
  { url :: Text,
    fileType :: S3.FileType
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
    shareable :: Bool,
    alwaysTriggerOnOnboarding :: Bool,
    messageId :: Id Domain.Message,
    mediaFiles :: [MediaFileApiResponse]
  }
  deriving (Generic, ToSchema, FromJSON)

instance ToJSON MessageAPIEntityResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype MessageReplyReq = MessageReplyReq {reply :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

messageList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> Flow [MessageAPIEntityResponse]
messageList (driverId, _, _) mbLimit mbOffset = do
  person <- B.runInReplica (QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId))
  messageDetails <- B.runInReplica $ MRQ.findByDriverIdAndLanguage (cast driverId) (fromMaybe ENGLISH person.language) mbLimit mbOffset
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
            shareable = rawMessage.shareable,
            alwaysTriggerOnOnboarding = rawMessage.alwaysTriggerOnOnboarding,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

getMessage :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.Message -> Flow MessageAPIEntityResponse
getMessage (driverId, _, _) messageId = do
  person <- B.runInReplica (QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId))
  messageDetails <- B.runInReplica $ MRQ.findByDriverIdMessageIdAndLanguage (cast driverId) messageId (fromMaybe ENGLISH person.language) >>= fromMaybeM (InvalidRequest "Message not found")
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
            shareable = rawMessage.shareable,
            alwaysTriggerOnOnboarding = rawMessage.alwaysTriggerOnOnboarding,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

fetchMedia :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> Flow Text
fetchMedia (driverId, _, _) filePath = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  S3.get $ T.unpack filePath

messageSeen :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.Message -> Flow APISuccess
messageSeen (driverId, _, _) messageId = do
  messageDetails <- B.runInReplica $ MRQ.findByMessageIdAndDriverId messageId (cast driverId) >>= fromMaybeM (InvalidRequest "Message not found")
  when (not messageDetails.readStatus) $ MQ.updateMessageViewCount messageId 1
  _ <- MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True Nothing
  return Success

messageLiked :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.Message -> Flow APISuccess
messageLiked (driverId, _, _) messageId = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  messageDetails <- B.runInReplica $ MRQ.findByMessageIdAndDriverId messageId (cast driverId) >>= fromMaybeM (InvalidRequest "Message not found")
  unless (messageDetails.readStatus) $
    throwError $ InvalidRequest "Message is not seen"
  let val = if messageDetails.likeStatus then (-1) else 1
  when messageDetails.readStatus $ MQ.updateMessageLikeCount messageId val
  MRQ.updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId (cast driverId)
  return Success

messageResponse :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Domain.Message -> MessageReplyReq -> Flow APISuccess
messageResponse (driverId, _, _) messageId MessageReplyReq {..} = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  _ <- MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True (Just reply)
  return Success
