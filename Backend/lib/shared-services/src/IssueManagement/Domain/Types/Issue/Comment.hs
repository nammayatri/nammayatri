module IssueManagement.Domain.Types.Issue.Comment where

import Data.Time
import EulerHS.Prelude hiding (id)
import IssueManagement.Common as Reexport
import qualified IssueManagement.Domain.Types.Issue.IssueReport as D
import Kernel.Types.Id

data Comment = Comment
  { id :: Id Comment,
    issueReportId :: Id D.IssueReport,
    authorId :: Id Person,
    comment :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
