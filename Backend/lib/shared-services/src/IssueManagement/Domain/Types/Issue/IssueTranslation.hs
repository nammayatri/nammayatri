module IssueManagement.Domain.Types.Issue.IssueTranslation where

import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language)
import Kernel.Types.Id

data IssueTranslation = IssueTranslation
  { id :: Id IssueTranslation,
    sentence :: Text,
    translation :: Text,
    language :: Language
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
