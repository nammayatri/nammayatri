module Storage.Queries.Issue.IssueReport where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueReport
import qualified Domain.Types.Person as SP
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueReport as BeamIR
import Storage.Tabular.Issue.IssueReport

create :: IssueReport -> SqlDB ()
create = Esq.create

findAllWithOptions :: Transactionable m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee = Esq.findAll $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    whenJust_ mbStatus (\statusVal -> issueReport ^. IssueReportStatus ==. val statusVal)
      &&. whenJust_ mbAssignee (\assignee -> issueReport ^. IssueReportAssignee ==. just (val assignee))
      &&. whenJust_ mbCategoryId (\categoryId -> issueReport ^. IssueReportCategoryId ==. val (toKey categoryId))
  orderBy [desc $ issueReport ^. IssueReportCreatedAt]
  limit limitVal
  offset offsetVal
  return issueReport
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

findById :: Transactionable m => Id IssueReport -> m (Maybe IssueReport)
findById issueReportId = Esq.findOne $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    issueReport ^. IssueReportTId ==. val (toKey issueReportId)
      &&. issueReport ^. IssueReportDeleted ==. val False
  pure issueReport

findAllByDriver :: Id SP.Person -> Transactionable m => m [IssueReport]
findAllByDriver driverId = Esq.findAll $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    issueReport ^. IssueReportDriverId ==. val (toKey driverId)
      &&. issueReport ^. IssueReportDeleted ==. val False
  pure issueReport

safeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m (Maybe IssueReport)
safeToDelete issueReportId driverId = Esq.findOne $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    issueReport ^. IssueReportTId ==. val (toKey issueReportId)
      &&. issueReport ^. IssueReportDeleted ==. val False
      &&. issueReport ^. IssueReportDriverId ==. val (toKey driverId)
  pure issueReport

isSafeToDelete :: Transactionable m => Id IssueReport -> Id SP.Person -> m Bool
isSafeToDelete issueReportId driverId = do
  findSafeToDelete <- safeToDelete issueReportId driverId
  return $ isJust findSafeToDelete

deleteByPersonId :: Id SP.Person -> SqlDB ()
deleteByPersonId driverId =
  Esq.delete $ do
    issueReport <- from $ table @IssueReportT
    where_ $ issueReport ^. IssueReportDriverId ==. val (toKey driverId)

deleteByPersonId' :: L.MonadFlow m => Id SP.Person -> m ()
deleteByPersonId' (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamIR.driverId (Se.Eq driverId)]
    Nothing -> pure ()

updateAsDeleted :: Id IssueReport -> SqlDB ()
updateAsDeleted issueReportId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportDeleted =. val True,
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateStatusAssignee :: Id IssueReport -> Maybe IssueStatus -> Maybe Text -> SqlDB ()
updateStatusAssignee issueReportId status assignee = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      ( [IssueReportUpdatedAt =. val now]
          <> maybe [] (\justStatus -> [IssueReportStatus =. val justStatus]) status
          <> maybe [] (\justAssignee -> [IssueReportAssignee =. just (val justAssignee)]) assignee
      )
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)
        &&. tbl ^. IssueReportDeleted ==. val False

updateOption :: Id IssueReport -> Id IssueOption -> SqlDB ()
updateOption issueReportId optionId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IssueReportOptionId =. just (val (toKey optionId)),
        IssueReportUpdatedAt =. val now
      ]
    where_ $
      tbl ^. IssueReportTId ==. val (toKey issueReportId)
        &&. tbl ^. IssueReportDeleted ==. val False

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
