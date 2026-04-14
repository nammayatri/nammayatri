{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.ChatMessage where

import qualified Data.Time as T
import IssueManagement.Domain.Types.Issue.ChatMessage as ChatMessage
import IssueManagement.Domain.Types.Issue.IssueReport (IssueReport)
import qualified IssueManagement.Storage.Beam.Issue.ChatMessage as BeamCM
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => ChatMessage.ChatMessage -> m ()
create = createWithKV

findChatMessagesAfter :: BeamFlow m r => Id IssueReport -> Maybe UTCTime -> Maybe Int -> m [ChatMessage]
findChatMessagesAfter (Id issueReportId) mbSince mbLimit = do
  let sinceCond = case mbSince of
        Nothing -> []
        Just ts -> [Is BeamCM.createdAt $ GreaterThan (T.utcToLocalTime T.utc ts)]
  findAllWithOptionsKV
    [And ([Is BeamCM.issueReportId $ Eq issueReportId] <> sinceCond)]
    (Asc BeamCM.createdAt)
    mbLimit
    Nothing

markReadUpTo :: BeamFlow m r => Id IssueReport -> ChatSenderType -> UTCTime -> m ()
markReadUpTo (Id issueReportId) fromSender upTo = do
  now <- getCurrentTime
  updateWithKV
    [Set BeamCM.readAt (Just (T.utcToLocalTime T.utc now))]
    [ And
        [ Is BeamCM.issueReportId $ Eq issueReportId,
          Is BeamCM.senderType $ Eq fromSender,
          Is BeamCM.readAt $ Eq Nothing,
          Is BeamCM.createdAt $ LessThanOrEq (T.utcToLocalTime T.utc upTo)
        ]
    ]

countUnread :: BeamFlow m r => Id IssueReport -> ChatSenderType -> m Int
countUnread (Id issueReportId) fromSender = do
  (rows :: [ChatMessage]) <-
    findAllWithKV
      [ And
          [ Is BeamCM.issueReportId $ Eq issueReportId,
            Is BeamCM.senderType $ Eq fromSender,
            Is BeamCM.readAt $ Eq Nothing
          ]
      ]
  pure (length rows)

instance FromTType' BeamCM.ChatMessage ChatMessage where
  fromTType' BeamCM.ChatMessageT {..} = do
    pure $
      Just
        ChatMessage
          { id = Id id,
            issueReportId = Id issueReportId,
            senderId = Id senderId,
            mediaFileIds = map Id mediaFileIds,
            readAt = T.localTimeToUTC T.utc <$> readAt,
            createdAt = T.localTimeToUTC T.utc createdAt,
            merchantId = Id <$> merchantId,
            ..
          }

instance ToTType' BeamCM.ChatMessage ChatMessage where
  toTType' ChatMessage {..} = do
    BeamCM.ChatMessageT
      { BeamCM.id = getId id,
        BeamCM.issueReportId = getId issueReportId,
        BeamCM.senderId = getId senderId,
        BeamCM.senderType = senderType,
        BeamCM.chatContentType = chatContentType,
        BeamCM.message = message,
        BeamCM.mediaFileIds = map getId mediaFileIds,
        BeamCM.readAt = T.utcToLocalTime T.utc <$> readAt,
        BeamCM.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamCM.merchantId = getId <$> merchantId
      }
