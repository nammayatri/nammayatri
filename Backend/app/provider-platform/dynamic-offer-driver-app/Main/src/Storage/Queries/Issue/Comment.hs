module Storage.Queries.Issue.Comment where

import Domain.Types.Issue.Comment
import Domain.Types.Issue.IssueReport(IssueReport)
import Kernel.Types.Id
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Issue.Comment

create :: Comment -> SqlDB ()
create = Esq.create

findAllByIssueReportId :: Transactionable m => Id IssueReport -> m [Comment]
findAllByIssueReportId issueReportId = findAll $ do
    comment <- from $ table @CommentT
    where_ $ comment ^. CommentIssueReportId ==. val (toKey issueReportId)
    orderBy [desc $ comment ^. CommentCreatedAt]
    return comment