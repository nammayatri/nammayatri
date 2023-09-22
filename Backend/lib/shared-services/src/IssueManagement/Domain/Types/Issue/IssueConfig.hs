module IssueManagement.Domain.Types.Issue.IssueConfig where

import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.Issue.IssueMessage
import Kernel.Prelude
import Kernel.Types.Id

data IssueConfig = IssueConfig
  { id :: Id IssueConfig,
    autoMarkIssueResolveDuration :: Double,
    onAutoMarkIssueResMsgs :: [Id IssueMessage],
    onCreateIssueMsgs :: [Id IssueMessage],
    onIssueReopenMsgs :: [Id IssueMessage],
    onKaptMarkIssueAwtMsgs :: [Id IssueMessage]
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)
