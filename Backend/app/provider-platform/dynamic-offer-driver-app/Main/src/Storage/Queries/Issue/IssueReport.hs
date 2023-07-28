{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.IssueReport where

import qualified Data.Time as T
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueReport as IssueReport
import qualified Domain.Types.Person as SP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Kernel.Utils.Common (MonadTime (..), getCurrentTime)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, deleteWithKV, findAllWithKV, findAllWithOptionsKV, findAllWithOptionsKvInReplica, findOneWithKV, findOneWithKvInReplica, updateOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueReport as BeamIR

-- create :: IssueReport -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => IssueReport.IssueReport -> m ()
create = createWithKV

-- findAllWithOptions :: Transactionable m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
-- findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee = Esq.findAll $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     whenJust_ mbStatus (\statusVal -> issueReport ^. IssueReportStatus ==. val statusVal)
--       &&. whenJust_ mbAssignee (\assignee -> issueReport ^. IssueReportAssignee ==. just (val assignee))
--       &&. whenJust_ mbCategoryId (\categoryId -> issueReport ^. IssueReportCategoryId ==. val (toKey categoryId))
--   orderBy [desc $ issueReport ^. IssueReportCreatedAt]
--   limit limitVal
--   offset offsetVal
--   return issueReport
--   where
--     limitVal = min (maybe 10 fromIntegral mbLimit) 10
--     offsetVal = maybe 0 fromIntegral mbOffset

findAllWithOptions :: (L.MonadFlow m, Log m) => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
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

findAllWithOptionsInReplica :: (L.MonadFlow m, Log m) => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
findAllWithOptionsInReplica mbLimit mbOffset mbStatus mbCategoryId mbAssignee = do
  findAllWithOptionsKvInReplica conditions (Se.Desc BeamIR.createdAt) (Just limitVal) (Just offsetVal)
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

-- findById :: Transactionable m => Id IssueReport -> m (Maybe IssueReport)
-- findById issueReportId = Esq.findOne $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportTId ==. val (toKey issueReportId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--   pure issueReport

findById :: (L.MonadFlow m, Log m) => Id IssueReport -> m (Maybe IssueReport)
findById (Id issueReportId) = findOneWithKV [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.deleted $ Se.Eq False]]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id IssueReport -> m (Maybe IssueReport)
findByIdInReplica (Id issueReportId) = findOneWithKvInReplica [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.deleted $ Se.Eq False]]

-- findAllByDriver :: Id SP.Person -> Transactionable m => m [IssueReport]
-- findAllByDriver driverId = Esq.findAll $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportDriverId ==. val (toKey driverId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--   pure issueReport

findAllByDriver :: (L.MonadFlow m, Log m) => Id SP.Person -> m [IssueReport]
findAllByDriver (Id driverId) = findAllWithKV [Se.And [Se.Is BeamIR.driverId $ Se.Eq driverId, Se.Is BeamIR.deleted $ Se.Eq False]]

-- safeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
-- safeToDelete issueReportId driverId = Esq.findOne $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportTId ==. val (toKey issueReportId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--       &&. issueReport ^. IssueReportDriverId ==. val (toKey driverId)
--   pure issueReport

safeToDelete :: (L.MonadFlow m, Log m) => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
safeToDelete (Id issueReportId) (Id driverId) = findOneWithKV [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.driverId $ Se.Eq driverId, Se.Is BeamIR.deleted $ Se.Eq False]]

safeToDeleteInReplica :: (L.MonadFlow m, Log m) => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
safeToDeleteInReplica (Id issueReportId) (Id driverId) = findOneWithKvInReplica [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.driverId $ Se.Eq driverId, Se.Is BeamIR.deleted $ Se.Eq False]]

-- isSafeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m Bool
-- isSafeToDelete issueReportId driverId = do
--   findSafeToDelete <- safeToDelete issueReportId driverId
--   return $ isJust findSafeToDelete

isSafeToDelete :: (L.MonadFlow m, Log m) => Id IssueReport -> Id SP.Person -> m Bool
isSafeToDelete issueReportId driverId = do
  findSafeToDelete <- safeToDelete issueReportId driverId
  return $ isJust findSafeToDelete

isSafeToDeleteInReplica :: (L.MonadFlow m, Log m) => Id IssueReport -> Id SP.Person -> m Bool
isSafeToDeleteInReplica issueReportId driverId = do
  findSafeToDelete <- safeToDeleteInReplica issueReportId driverId
  return $ isJust findSafeToDelete

-- deleteByPersonId :: Id SP.Person -> SqlDB ()
-- deleteByPersonId driverId =
--   Esq.delete $ do
--     issueReport <- from $ table @IssueReportT
--     where_ $ issueReport ^. IssueReportDriverId ==. val (toKey driverId)

deleteByPersonId :: (L.MonadFlow m, Log m) => Id SP.Person -> m ()
deleteByPersonId (Id driverId) = deleteWithKV [Se.Is BeamIR.driverId (Se.Eq driverId)]

-- updateAsDeleted :: Id IssueReport -> SqlDB ()
-- updateAsDeleted issueReportId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ IssueReportDeleted =. val True,
--         IssueReportUpdatedAt =. val now
--       ]
--     where_ $
--       tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateAsDeleted :: (L.MonadFlow m, MonadTime m, Log m) => Id IssueReport -> m ()
updateAsDeleted issueReportId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamIR.deleted True,
      Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

-- updateStatusAssignee :: Id IssueReport -> Maybe IssueStatus -> Maybe Text -> SqlDB ()
-- updateStatusAssignee issueReportId status assignee = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       ( [IssueReportUpdatedAt =. val now]
--           <> maybe [] (\justStatus -> [IssueReportStatus =. val justStatus]) status
--           <> maybe [] (\justAssignee -> [IssueReportAssignee =. just (val justAssignee)]) assignee
--       )
--     where_ $
--       tbl ^. IssueReportTId ==. val (toKey issueReportId)
--         &&. tbl ^. IssueReportDeleted ==. val False

updateStatusAssignee :: (L.MonadFlow m, MonadTime m, Log m) => Id IssueReport -> Maybe IssueStatus -> Maybe Text -> m ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  updateOneWithKV
    ([Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now] <> if isJust status then [Se.Set BeamIR.status (fromJust status)] else [] <> ([Se.Set BeamIR.assignee assignee | isJust assignee]))
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

-- updateOption :: Id IssueReport -> Id IssueOption -> SqlDB ()
-- updateOption issueReportId optionId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ IssueReportOptionId =. just (val (toKey optionId)),
--         IssueReportUpdatedAt =. val now
--       ]
--     where_ $
--       tbl ^. IssueReportTId ==. val (toKey issueReportId)
--         &&. tbl ^. IssueReportDeleted ==. val False

updateOption :: (L.MonadFlow m, MonadTime m, Log m) => Id IssueReport -> Id IssueOption -> m ()
updateOption issueReportId (Id optionId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamIR.optionId (Just optionId), Se.Set BeamIR.updatedAt $ T.utcToLocalTime T.utc now]
    [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]

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
        BeamIR.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamIR.updatedAt = T.utcToLocalTime T.utc updatedAt
      }
