module IssueManagement.Domain.Types.Issue.IssueTranslation where

import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.External.Types (Language)
import Kernel.Types.Id
import Kernel.Utils.Common

data IssueTranslation = IssueTranslation
  { id :: Id IssueTranslation,
    sentence :: Text,
    translation :: Text,
    language :: Language,
    merchantId :: Id Merchant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data DetailedTranslation = DetailedTranslation
  { titleTranslation :: Maybe IssueTranslation,
    contentTranslation :: Maybe IssueTranslation,
    actionTranslation :: Maybe IssueTranslation
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
