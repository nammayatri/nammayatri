module Domain.Types.Issue.IssueOption where

import Domain.Types.Issue.IssueCategory (IssueCategory)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data IssueOption = IssueOption
  { id :: Id IssueOption,
    issueCategoryId :: Id IssueCategory,
    option :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
