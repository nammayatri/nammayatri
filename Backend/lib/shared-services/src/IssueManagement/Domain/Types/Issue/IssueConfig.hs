module IssueManagement.Domain.Types.Issue.IssueConfig where

import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.Issue.IssueMessage
import Kernel.Prelude
import Kernel.Types.Id

data IssueConfig = IssueConfig
  { id :: Id IssueConfig,
    autoMarkIssueClosedDuration :: Double,
    onAutoMarkIssueClsMsgs :: [Id IssueMessage],
    onCreateIssueMsgs :: [Id IssueMessage],
    onIssueReopenMsgs :: [Id IssueMessage],
    onKaptMarkIssueResMsgs :: [Id IssueMessage]
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)
