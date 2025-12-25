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
    isActive :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

data IssueMessageType = Intermediate | Terminal | FAQ
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

$(mkBeamInstancesForEnum ''IssueMessageType)
