module Storage.Queries.Issue.IssueReport where

import Domain.Types.Issue.IssueReport
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Issue.IssueReport

create :: IssueReport -> SqlDB ()
create = Esq.create

findAll :: Transactionable m => m [IssueReport]
findAll = Esq.findAll $ do
    issueReport <- from $ table @IssueReportT
    pure issueReport

findAllByDriver :: Id SP.Person -> Transactionable m => m [IssueReport]
findAllByDriver driverId = Esq.findAll $ do
    issueReport <- from $ table @IssueReportT
    where_ $
        issueReport ^. IssueReportDriverId ==. val (toKey driverId)
        &&. issueReport ^. IssueReportDeleted ==. val False
    pure issueReport

findAllWithLimitOffsetStatus :: Transactionable m => Maybe Int -> Maybe Int -> Maybe IssueStatus -> m [IssueReport]
findAllWithLimitOffsetStatus mbLimit mbOffset mbStatus = Esq.findAll $ do
    issueReport <- from $ table @IssueReportT
    where_ $
        issueReport ^. IssueReportStatus ==. val statusVal
        &&. issueReport ^. IssueReportDeleted ==. val False
    orderBy [desc $ issueReport ^. IssueReportCreatedAt]
    limit limitVal
    offset offsetVal
    return issueReport
    where
        limitVal = min (maybe 10 fromIntegral mbLimit) 10
        offsetVal = maybe 0 fromIntegral mbOffset
        statusVal = fromMaybe NEW mbStatus

updateAsDeleted :: Id IssueReport -> Id SP.Person -> SqlDB ()
updateAsDeleted issueReportId driverId = do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
        set
            tbl
            [ IssueReportDeleted =. val True,
              IssueReportUpdatedAt =. val now
            ]
        where_ $
            tbl ^. IssueReportTId ==. val (toKey issueReportId)
            &&. tbl ^. IssueReportDriverId ==. val (toKey driverId)

updateStatus :: Id IssueReport -> IssueStatus -> SqlDB ()
updateStatus issueReportId status = do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
        set
            tbl
            [ IssueReportStatus =. val status,
              IssueReportUpdatedAt =. val now
            ]
        where_ $
            tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateAssignee :: Id IssueReport -> Text -> SqlDB ()
updateAssignee issueReportId assignee = do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
        set
            tbl
            [ IssueReportAssignee =. just(val assignee),
              IssueReportUpdatedAt =. val now
            ]
        where_ $
            tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateStatusAssignee :: Id IssueReport -> IssueStatus -> Text -> SqlDB ()
updateStatusAssignee issueReportId status assignee = do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
        set
            tbl
            [ IssueReportStatus =. val status,
              IssueReportAssignee =. just(val assignee),
              IssueReportUpdatedAt =. val now
            ]
        where_ $
            tbl ^. IssueReportTId ==. val (toKey issueReportId)

updateOption :: Id IssueReport -> Text -> SqlDB ()
updateOption issueReportId option = do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
        set
            tbl
            [ IssueReportOption =. just(val option),
              IssueReportUpdatedAt =. val now
            ]
        where_ $
            tbl ^. IssueReportTId ==. val (toKey issueReportId)