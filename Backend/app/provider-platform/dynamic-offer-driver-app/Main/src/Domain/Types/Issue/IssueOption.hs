module Domain.Types.Issue.IssueOption where

import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Domain.Types.Issue.IssueCategory (IssueCategory)

data IssueOption = IssueOption
  { id :: Id IssueOption,
    issueCategoryId :: Id IssueCategory,
    option :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
