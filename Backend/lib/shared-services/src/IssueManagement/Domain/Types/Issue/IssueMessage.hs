module IssueManagement.Domain.Types.Issue.IssueMessage where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Lib.UtilsTH
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

data IssueMessage = IssueMessage
  { id :: Id IssueMessage,
    categoryId :: Maybe (Id IssueCategory),
    optionId :: Maybe (Id IssueOption),
    merchantOperatingCityId :: Id MerchantOperatingCity,
    message :: Text,
    priority :: Int,
    label :: Maybe Text,
    merchantId :: Id Merchant,
    referenceOptionId :: Maybe (Id IssueOption),
    referenceCategoryId :: Maybe (Id IssueCategory),
    mediaFiles :: [Id MediaFile],
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    messageType :: IssueMessageType,
    -- | JSON-encoded 'ApiCallAction'. Present only when messageType is ApiCall:
    -- instead of being shown to the user, this message runs a declared lookup
    -- (IssueApiIntegration) and the flow auto-descends into the branch option whose
    -- condition matches the extracted response fields.
    apiAction :: Maybe Text,
    isActive :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

data IssueMessageType = Intermediate | Terminal | FAQ | ApiCall
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

$(mkBeamInstancesForEnum ''IssueMessageType)

-- | Decoded shape of 'apiAction'. Branch targets are IssueOption ids: taking a
-- branch behaves exactly as if the user had tapped that option, so the whole
-- existing option->childMessages machinery (and the tree editor) is reused.
-- 'onErrorNextOptionId' is mandatory so a failing lookup can never dead-end
-- a conversation.
data ApiCallAction = ApiCallAction
  { integrationId :: Text,
    -- | placeholder name -> flow-context variable name. When omitted, every
    -- context variable is offered to the integration under its own name.
    paramMapping :: Maybe (Map Text Text),
    branches :: [ApiCallBranch],
    defaultNextOptionId :: Maybe Text,
    onErrorNextOptionId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

data ApiCallBranch = ApiCallBranch
  { -- | json-logic rule evaluated over the flow context (extracted fields +
    -- builtins). First branch whose rule holds wins.
    condition :: Value,
    nextOptionId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
