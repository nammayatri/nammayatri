module Domain.Types.Issue.Comment where

import Data.Time
import qualified Domain.Types.Issue.IssueReport as D
import qualified Domain.Types.Person as D
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data Comment = Comment
  { id :: Id Comment,
    issueReportId :: Id D.IssueReport,
    authorId :: Id D.Person,
    comment :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
