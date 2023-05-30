module Storage.Queries.Issue.IssueReport where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueReport as IssueReport
import qualified Domain.Types.Person as SP
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadTime (..), getCurrentTime)
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueReport as BeamIR

-- create :: IssueReport -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => IssueReport.IssueReport -> m (MeshResult ())
create issueReport = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainIssueReportToBeam issueReport)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

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

findAllWithOptions :: L.MonadFlow m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithOptionsKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamIR.status $ Se.Eq (fromJust mbStatus), Se.Is BeamIR.categoryId $ Se.Eq (fromJust (getId <$> mbCategoryId)), Se.Is BeamIR.assignee $ Se.Eq mbAssignee]] (Se.Desc BeamIR.createdAt) (Just limitVal) (Just offsetVal)
      case result of
        Left _ -> pure []
        Right issueReport -> pure $ transformBeamIssueReportToDomain <$> issueReport
    Nothing -> pure []
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

-- findById :: Transactionable m => Id IssueReport -> m (Maybe IssueReport)
-- findById issueReportId = Esq.findOne $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportTId ==. val (toKey issueReportId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--   pure issueReport

findById :: L.MonadFlow m => Id IssueReport -> m (Maybe IssueReport)
findById (Id issueReportId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamIssueReportToDomain <$>) <$> KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamIR.id $ Se.Eq issueReportId]
    Nothing -> pure Nothing

-- findAllByDriver :: Id SP.Person -> Transactionable m => m [IssueReport]
-- findAllByDriver driverId = Esq.findAll $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportDriverId ==. val (toKey driverId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--   pure issueReport

findAllByDriver :: L.MonadFlow m => Id SP.Person -> m [IssueReport]
findAllByDriver (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamIssueReportToDomain <$>) <$> KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamIR.driverId $ Se.Eq driverId]
    Nothing -> pure []

-- safeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
-- safeToDelete issueReportId driverId = Esq.findOne $ do
--   issueReport <- from $ table @IssueReportT
--   where_ $
--     issueReport ^. IssueReportTId ==. val (toKey issueReportId)
--       &&. issueReport ^. IssueReportDeleted ==. val False
--       &&. issueReport ^. IssueReportDriverId ==. val (toKey driverId)
--   pure issueReport

safeToDelete :: L.MonadFlow m => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
safeToDelete (Id issueReportId) (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamIR.id $ Se.Eq issueReportId, Se.Is BeamIR.driverId $ Se.Eq driverId]]
      case result of
        Right issueReport -> pure $ transformBeamIssueReportToDomain <$> issueReport
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- isSafeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m Bool
-- isSafeToDelete issueReportId driverId = do
--   findSafeToDelete <- safeToDelete issueReportId driverId
--   return $ isJust findSafeToDelete

isSafeToDelete :: L.MonadFlow m => Id IssueReport -> Id SP.Person -> m Bool
isSafeToDelete issueReportId driverId = do
  findSafeToDelete <- safeToDelete issueReportId driverId
  return $ isJust findSafeToDelete

-- deleteByPersonId :: Id SP.Person -> SqlDB ()
-- deleteByPersonId driverId =
--   Esq.delete $ do
--     issueReport <- from $ table @IssueReportT
--     where_ $ issueReport ^. IssueReportDriverId ==. val (toKey driverId)

deleteByPersonId :: L.MonadFlow m => Id SP.Person -> m ()
deleteByPersonId (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamIR.driverId (Se.Eq driverId)]
    Nothing -> pure ()

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

updateAsDeleted :: (L.MonadFlow m, MonadTime m) => Id IssueReport -> m ()
updateAsDeleted issueReportId = do
  now <- getCurrentTime
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamIR.deleted True,
            Se.Set BeamIR.updatedAt now
          ]
          [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]
    Nothing -> pure ()

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

updateStatusAssignee :: (L.MonadFlow m, MonadTime m) => Id IssueReport -> Maybe IssueStatus -> Maybe Text -> m ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          ([Se.Set BeamIR.updatedAt now] <> if isJust status then [Se.Set BeamIR.status (fromJust status)] else [] <> ([Se.Set BeamIR.assignee assignee | isJust assignee]))
          [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]
    Nothing -> pure ()

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

updateOption :: (L.MonadFlow m, MonadTime m) => Id IssueReport -> Id IssueOption -> m ()
updateOption issueReportId (Id optionId) = do
  now <- getCurrentTime
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Set BeamIR.optionId (Just optionId), Se.Set BeamIR.updatedAt now]
          [Se.Is BeamIR.id (Se.Eq $ getId issueReportId)]
    Nothing -> pure ()

transformBeamIssueReportToDomain :: BeamIR.IssueReport -> IssueReport
transformBeamIssueReportToDomain BeamIR.IssueReportT {..} = do
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
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainIssueReportToBeam :: IssueReport -> BeamIR.IssueReport
transformDomainIssueReportToBeam IssueReport {..} =
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
      BeamIR.createdAt = createdAt,
      BeamIR.updatedAt = updatedAt
    }
