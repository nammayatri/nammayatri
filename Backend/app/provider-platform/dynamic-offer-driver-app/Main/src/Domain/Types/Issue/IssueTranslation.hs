module Domain.Types.Issue.IssueTranslation where

import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.External.Types (Language)

data IssueTranslation = IssueTranslation
  { id :: Id IssueTranslation,
    sentence :: Text,
    translation :: Text,
    language :: Language
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
