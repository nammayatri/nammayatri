module Domain.Types.Issue.Comment where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Domain.Types.Issue.IssueReport as D
import Kernel.Types.Id

data Comment = Comment
  { id :: Id Comment,
    issueReportId :: Id D.IssueReport,
    author :: Text,
    comment :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
