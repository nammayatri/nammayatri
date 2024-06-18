module IssueManagement.Domain.Types.Issue.IssueMessage where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueOption
import Kernel.Types.Id (Id)

data IssueMessage = IssueMessage
  { id :: Id IssueMessage,
    categoryId :: Maybe (Id IssueCategory),
    optionId :: Maybe (Id IssueOption),
    message :: Text,
    priority :: Int,
    label :: Maybe Text,
    merchantId :: Id Merchant
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
