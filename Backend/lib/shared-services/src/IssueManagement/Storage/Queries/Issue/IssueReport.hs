{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueReport where

import qualified Data.Time as T
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport as IssueReport
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as BeamIR
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => IssueReport.IssueReport -> m ()
create = createWithKV

findAllWithOptions :: BeamFlow m r => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Maybe (Id Person) -> Maybe (Id Ride) -> m [IssueReport]
findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee mbPersonId mbRideId = do
  findAllWithOptionsKV conditions (Desc BeamIR.createdAt) (Just limitVal) (Just offsetVal)
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset
    conditions =
      [ And $
          catMaybes
            [ fmap (Is BeamIR.status . Eq) mbStatus,
              fmap (Is BeamIR.assignee . Eq . Just) mbAssignee,
              fmap (Is BeamIR.categoryId . Eq . getId) mbCategoryId,
              fmap (Is BeamIR.personId . Eq . getId) mbPersonId,
              fmap (Is BeamIR.rideId . Eq . Just . getId) mbRideId
            ]
      ]

findById :: BeamFlow m r => Id IssueReport -> m (Maybe IssueReport)
findById (Id issueReportId) = findOneWithKV [And [Is BeamIR.id $ Eq issueReportId, Is BeamIR.deleted $ Eq False]]

findByShortId :: BeamFlow m r => ShortId IssueReport -> m (Maybe IssueReport)
findByShortId (ShortId issueReportShortId) = findOneWithKV [And [Is BeamIR.shortId $ Eq $ Just issueReportShortId, Is BeamIR.deleted $ Eq False]]

findAllByPerson :: BeamFlow m r => Id Person -> m [IssueReport]
findAllByPerson (Id personId) = findAllWithOptionsKV [And [Is BeamIR.personId $ Eq personId, Is BeamIR.deleted $ Eq False]] (Desc BeamIR.updatedAt) Nothing Nothing

findAllByPersonAndRideId :: BeamFlow m r => Id Person -> Id Ride -> m [IssueReport]
findAllByPersonAndRideId (Id personId) (Id rideId) = findAllWithOptionsKV [And [Is BeamIR.personId $ Eq personId, Is BeamIR.rideId $ Eq (Just rideId), Is BeamIR.deleted $ Eq False]] (Desc BeamIR.updatedAt) Nothing Nothing

safeToDelete :: BeamFlow m r => Id IssueReport -> Id Person -> m (Maybe IssueReport)
safeToDelete (Id issueReportId) (Id personId) = findOneWithKV [And [Is BeamIR.id $ Eq issueReportId, Is BeamIR.personId $ Eq personId, Is BeamIR.deleted $ Eq False]]

isSafeToDelete :: BeamFlow m r => Id IssueReport -> Id Person -> m Bool
isSafeToDelete issueReportId personId = do
  findSafeToDelete <- safeToDelete issueReportId personId
  return $ isJust findSafeToDelete

deleteByPersonId :: BeamFlow m r => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Is BeamIR.personId (Eq personId)]

updateAsDeleted :: BeamFlow m r => Id IssueReport -> m ()
updateAsDeleted issueReportId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Set BeamIR.deleted True,
      Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Is BeamIR.id (Eq $ getId issueReportId)]

updateStatusAssignee :: BeamFlow m r => Id IssueReport -> Maybe IssueStatus -> Maybe Text -> m ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  updateOneWithKV
    ([Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now] <> if isJust status then [Set BeamIR.status (fromJust status)] else [] <> ([Set BeamIR.assignee assignee | isJust assignee]))
    [Is BeamIR.id (Eq $ getId issueReportId)]

updateOption :: BeamFlow m r => Id IssueReport -> Id IssueOption -> m ()
updateOption issueReportId (Id optionId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Set BeamIR.optionId (Just optionId), Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Is BeamIR.id (Eq $ getId issueReportId)]

updateIssueStatus :: BeamFlow m r => Text -> IssueStatus -> m ()
updateIssueStatus ticketId status = do
  now <- getCurrentTime
  updateOneWithKV
    [Set BeamIR.status status, Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Is BeamIR.ticketId (Eq (Just ticketId))]

updateIssueReopenedCount :: BeamFlow m r => Id IssueReport -> Int -> m ()
updateIssueReopenedCount ticketId reopenedCount = do
  now <- getCurrentTime
  updateOneWithKV
    [Set BeamIR.reopenedCount (Just reopenedCount), Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Is BeamIR.id (Eq $ getId ticketId)]

updateTicketId :: BeamFlow m r => Id IssueReport -> Text -> m ()
updateTicketId issueId ticketId = do
  now <- getCurrentTime
  updateOneWithKV
    [Set BeamIR.ticketId (Just ticketId), Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Is BeamIR.id (Eq $ getId issueId)]

findByTicketId :: BeamFlow m r => Text -> m (Maybe IssueReport)
findByTicketId ticketId = findOneWithKV [Is BeamIR.ticketId $ Eq (Just ticketId)]

updateChats :: BeamFlow m r => Id IssueReport -> [Chat] -> m ()
updateChats issueId chats = do
  now <- getCurrentTime
  updateOneWithKV
    [Set BeamIR.chats chats, Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Is BeamIR.id (Eq $ getId issueId)]

findByBecknIssueId :: BeamFlow m r => Text -> m (Maybe IssueReport)
findByBecknIssueId becknIssueId = findOneWithKV [Is BeamIR.becknIssueId $ Eq (Just becknIssueId)]

instance FromTType' BeamIR.IssueReport IssueReport where
  fromTType' BeamIR.IssueReportT {..} = do
    pure $
      Just
        IssueReport
          { id = Id id,
            shortId = ShortId <$> shortId,
            personId = Id personId,
            driverId = Id <$> driverId,
            rideId = Id <$> rideId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            categoryId = Id categoryId,
            optionId = Id <$> optionId,
            mediaFiles = Id <$> mediaFiles,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            merchantId = Id <$> merchantId,
            becknIssueId = becknIssueId,
            reopenedCount = fromMaybe 0 reopenedCount,
            ..
          }

instance ToTType' BeamIR.IssueReport IssueReport where
  toTType' IssueReport {..} = do
    BeamIR.IssueReportT
      { BeamIR.id = getId id,
        BeamIR.shortId = getShortId <$> shortId,
        BeamIR.personId = getId personId,
        BeamIR.driverId = getId <$> driverId,
        BeamIR.rideId = getId <$> rideId,
        BeamIR.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        BeamIR.description = description,
        BeamIR.assignee = assignee,
        BeamIR.status = status,
        BeamIR.categoryId = getId categoryId,
        BeamIR.optionId = getId <$> optionId,
        BeamIR.deleted = deleted,
        BeamIR.mediaFiles = getId <$> mediaFiles,
        BeamIR.ticketId = ticketId,
        BeamIR.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamIR.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamIR.chats = chats,
        BeamIR.merchantId = getId <$> merchantId,
        BeamIR.becknIssueId = becknIssueId,
        BeamIR.reopenedCount = Just reopenedCount
      }
