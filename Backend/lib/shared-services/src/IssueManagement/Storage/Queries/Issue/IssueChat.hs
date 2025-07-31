{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueChat where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import qualified Data.Vector as V
import IssueManagement.Common (Person)
import IssueManagement.Domain.Types.Issue.IssueChat as IssueChat
import IssueManagement.Domain.Types.Issue.IssueReport
import qualified IssueManagement.Storage.Beam.Issue.IssueChat as BeamIC
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import qualified Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.Types.Id

create :: BeamFlow m r => IssueChat.IssueChat -> m ()
create = createWithKV

findById :: BeamFlow m r => Id IssueChat -> m (Maybe IssueChat)
findById (Id issueChatId) = findOneWithKV [Is BeamIC.id $ Eq issueChatId]

findByTicketId :: BeamFlow m r => Text -> m (Maybe IssueChat)
findByTicketId ticketId = findOneWithKV [Is BeamIC.ticketId $ Eq ticketId]

findAllByPersonId :: BeamFlow m r => Id Person -> m [IssueChat]
findAllByPersonId (Id personId) = findAllWithKV [Is BeamIC.personId $ Eq personId]

updateChats :: BeamFlow m r => Text -> [Text] -> [Text] -> m ()
updateChats tId chats mediaFiles = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIC.chats chats,
      Set BeamIC.mediaFiles mediaFiles,
      Set BeamIC.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Is BeamIC.ticketId $ Eq tId]

updateChatsWithKaptureData :: BeamFlow m r => Text -> [Text] -> [Text] -> Maybe [Kapture.ChatMessage] -> m ()
updateChatsWithKaptureData tId chats mediaFiles kaptureData = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIC.chats chats,
      Set BeamIC.mediaFiles mediaFiles,
      Set BeamIC.kaptureData (encodeKaptureData kaptureData),
      Set BeamIC.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Is BeamIC.ticketId $ Eq tId]
  where
    encodeKaptureData :: Maybe [Kapture.ChatMessage] -> Maybe Text
    encodeKaptureData = fmap (TE.decodeUtf8 . LBS.toStrict . A.encode)

updateIssueReportId :: BeamFlow m r => Text -> Maybe (Id IssueReport) -> m ()
updateIssueReportId tId issueReportId = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIC.issueReportId (getId <$> issueReportId),
      Set BeamIC.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Is BeamIC.ticketId $ Eq tId]

instance FromTType' BeamIC.IssueChat IssueChat where
  fromTType' BeamIC.IssueChatT {..} = do
    pure $
      Just
        IssueChat
          { id = Id id,
            rideId = Id <$> rideId,
            personId = Id personId,
            kaptureData = decodeKaptureData kaptureData,
            issueReportId = Id <$> issueReportId,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            ..
          }
    where
      decodeKaptureData :: Maybe Text -> Maybe [Kapture.ChatMessage]
      decodeKaptureData Nothing = Nothing
      decodeKaptureData (Just textData) =
        case A.decode (LBS.fromStrict . TE.encodeUtf8 $ textData) of
          Just (A.Array messages) ->
            let convertMessage :: A.Value -> Maybe Kapture.ChatMessage
                convertMessage (A.Object obj) =
                  let transformedObj =
                        A.Object $
                          KM.fromList
                            [ (Key.fromString "chat_message", fromMaybe (A.String "") (KM.lookup (Key.fromString "chatMessage") obj)),
                              (Key.fromString "sender_name", fromMaybe (A.String "") (KM.lookup (Key.fromString "senderName") obj)),
                              (Key.fromString "receiver_name", fromMaybe (A.String "") (KM.lookup (Key.fromString "receiverName") obj)),
                              (Key.fromString "sentDate", fromMaybe (A.String "") (KM.lookup (Key.fromString "sentDate") obj))
                            ]
                   in case A.fromJSON transformedObj of
                        A.Success chatMessage -> Just chatMessage
                        A.Error _ -> Nothing
                convertMessage _ = Nothing
             in sequence (map convertMessage (V.toList messages))
          _ -> Nothing

instance ToTType' BeamIC.IssueChat IssueChat where
  toTType' IssueChat {..} = do
    BeamIC.IssueChatT
      { BeamIC.id = getId id,
        BeamIC.rideId = getId <$> rideId,
        BeamIC.personId = getId personId,
        BeamIC.kaptureData = encodeKaptureData kaptureData,
        BeamIC.issueReportId = getId <$> issueReportId,
        BeamIC.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamIC.updatedAt = T.utcToLocalTime T.utc updatedAt,
        ..
      }
    where
      encodeKaptureData :: Maybe [Kapture.ChatMessage] -> Maybe Text
      encodeKaptureData Nothing = Nothing
      encodeKaptureData (Just chatMessages) =
        let transformMessage :: Kapture.ChatMessage -> A.Value
            transformMessage msg =
              A.Object $
                KM.fromList
                  [ (Key.fromString "chatMessage", A.toJSON msg.chatMessage),
                    (Key.fromString "senderName", A.String msg.senderName),
                    (Key.fromString "receiverName", A.String msg.receiverName),
                    (Key.fromString "sentDate", A.String msg.sentDate)
                  ]
            transformedMessages = A.Array $ V.fromList (map transformMessage chatMessages)
         in Just (TE.decodeUtf8 . LBS.toStrict . A.encode $ transformedMessages)
