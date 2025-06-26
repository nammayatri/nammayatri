{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueChat where

import qualified Data.Time as T
import IssueManagement.Domain.Types.Issue.IssueChat as IssueChat
import IssueManagement.Domain.Types.Issue.IssueReport
import qualified IssueManagement.Storage.Beam.Issue.IssueChat as BeamIC
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => IssueChat.IssueChat -> m ()
create = createWithKV

findById :: BeamFlow m r => Id IssueChat -> m (Maybe IssueChat)
findById (Id issueChatId) = findOneWithKV [Is BeamIC.id $ Eq issueChatId]

findByTicketId :: BeamFlow m r => Text -> m (Maybe IssueChat)
findByTicketId ticketId = findOneWithKV [Is BeamIC.ticketId $ Eq ticketId]

updateChats :: BeamFlow m r => Text -> [Text] -> [Text] -> m ()
updateChats tId chats mediaFiles = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIC.chats chats,
      Set BeamIC.mediaFiles mediaFiles,
      Set BeamIC.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Is BeamIC.ticketId $ Eq tId]

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
            issueReportId = Id <$> issueReportId,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            ..
          }

instance ToTType' BeamIC.IssueChat IssueChat where
  toTType' IssueChat {..} = do
    BeamIC.IssueChatT
      { BeamIC.id = getId id,
        BeamIC.rideId = getId <$> rideId,
        BeamIC.personId = getId personId,
        BeamIC.issueReportId = getId <$> issueReportId,
        BeamIC.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamIC.updatedAt = T.utcToLocalTime T.utc updatedAt,
        ..
      }
