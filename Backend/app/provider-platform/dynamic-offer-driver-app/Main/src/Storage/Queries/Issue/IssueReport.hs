{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.IssueReport where

import qualified Data.Time as T
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueReport as IssueReport
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueReport as BeamIR

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => IssueReport.IssueReport -> m ()
create = createWithKV

findAllWithOptions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee =
  findAllWithOptionsKV conditions (Se.Desc BeamIR.createdAt) (Just limitVal) (Just offsetVal)
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset
    conditions =
      [ Se.And $
          catMaybes
            [ fmap (Se.Is BeamIR.status . Se.Eq) mbStatus,
              fmap (Se.Is BeamIR.assignee . Se.Eq . Just) mbAssignee,
              fmap (Se.Is BeamIR.categoryId . Se.Eq . getId) mbCategoryId
            ]
      ]

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> m (Maybe IssueReport)
findById (Id issueReportId) = findOneWithKV [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.deleted $ Se.Eq False]]

findAllByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SP.Person -> m [IssueReport]
findAllByDriver (Id driverId) = findAllWithKV [Se.And [Se.Is BeamIR.driverId $ Se.Eq driverId, Se.Is BeamIR.deleted $ Se.Eq False]]

safeToDelete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
safeToDelete (Id issueReportId) (Id driverId) = findOneWithKV [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.driverId $ Se.Eq driverId, Se.Is BeamIR.deleted $ Se.Eq False]]

isSafeToDelete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> Id SP.Person -> m Bool
isSafeToDelete issueReportId driverId = do
  findSafeToDelete <- safeToDelete issueReportId driverId
  return $ isJust findSafeToDelete

deleteByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SP.Person -> m ()
deleteByPersonId (Id driverId) = deleteWithKV [Se.Is BeamIR.driverId (Se.Eq driverId)]

updateAsDeleted :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> m ()
updateAsDeleted issueReportId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamIR.deleted True,
      Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

updateStatusAssignee :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> Maybe IssueStatus -> Maybe Text -> m ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  updateOneWithKV
    ([Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now] <> if isJust status then [Se.Set BeamIR.status (fromJust status)] else [] <> ([Se.Set BeamIR.assignee assignee | isJust assignee]))
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

updateOption :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> Id IssueOption -> m ()
updateOption issueReportId (Id optionId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamIR.optionId (Just optionId), Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

updateIssueStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> IssueStatus -> m ()
updateIssueStatus ticketId status = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamIR.status status, Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Se.Is BeamIR.ticketId (Se.Eq (Just ticketId))]

updateTicketId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueReport -> Text -> m ()
updateTicketId issueId ticketId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamIR.ticketId (Just ticketId), Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Se.Is BeamIR.id (Se.Eq $ getId issueId)]

findByTicketId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe IssueReport)
findByTicketId ticketId = findOneWithKV [Se.Is BeamIR.ticketId $ Se.Eq (Just ticketId)]

instance FromTType' BeamIR.IssueReport IssueReport where
  fromTType' BeamIR.IssueReportT {..} = do
    pure $
      Just
        IssueReport
          { id = Id id,
            driverId = Id driverId,
            rideId = Id <$> rideId,
            description = description,
            assignee = assignee,
            status = status,
            categoryId = Id categoryId,
            optionId = Id <$> optionId,
            deleted = deleted,
            mediaFiles = Id <$> mediaFiles,
            ticketId = ticketId,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt
          }

instance ToTType' BeamIR.IssueReport IssueReport where
  toTType' IssueReport {..} = do
    BeamIR.IssueReportT
      { BeamIR.id = getId id,
        BeamIR.driverId = getId driverId,
        BeamIR.rideId = getId <$> rideId,
        BeamIR.description = description,
        BeamIR.assignee = assignee,
        BeamIR.status = status,
        BeamIR.categoryId = getId categoryId,
        BeamIR.optionId = getId <$> optionId,
        BeamIR.deleted = deleted,
        BeamIR.mediaFiles = getId <$> mediaFiles,
        BeamIR.ticketId = ticketId,
        BeamIR.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamIR.updatedAt = T.utcToLocalTime T.utc updatedAt
      }
