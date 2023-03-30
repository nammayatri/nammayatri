module Storage.Queries.Issue.IssueReport where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueReport
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Storage.Tabular.Issue.IssueReport

create :: IssueReport -> SqlDB ()
create = Esq.create

findAll :: Transactionable m => m [IssueReport]
findAll = Esq.findAll $ do
  from $ table @IssueReportT

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

findAllWithLimitOffsetStatus :: Transactionable m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> m [IssueReport]
findAllWithLimitOffsetStatus mbLimit mbOffset mbStatus mbCategoryId mbAssignee = Esq.findAll $ do
  issueReport <- from $ table @IssueReportT
  where_ $
    whenJust_ mbStatus (\statusVal -> issueReport ^. IssueReportStatus ==. val statusVal)
      &&. issueReport ^. IssueReportDeleted ==. val False
      &&. whenJust_ mbAssignee (\assignee -> issueReport ^. IssueReportAssignee ==. just (val assignee))
      &&. whenJust_ mbCategoryId (\categoryId -> issueReport ^. IssueReportCategoryId ==. val (toKey categoryId))
  orderBy [desc $ issueReport ^. IssueReportCreatedAt]
  limit limitVal
  offset offsetVal
  return issueReport
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

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
