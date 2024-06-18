module IssueManagement.Domain.Types.Issue.IssueTranslation where

import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.External.Types (Language)
import Kernel.Types.Id

data IssueTranslation = IssueTranslation
  { id :: Id IssueTranslation,
    sentence :: Text,
    translation :: Text,
    language :: Language,
    merchantId :: Id Merchant
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
