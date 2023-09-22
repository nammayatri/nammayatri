module IssueManagement.Domain.Types.Issue.IssueCategory where

import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data IssueCategory = IssueCategory
  { id :: Id IssueCategory,
    category :: Text,
    logoUrl :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
