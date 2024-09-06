module IssueManagement.Domain.Types.Issue.IssueOption where

import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import qualified IssueManagement.Common as Common
import IssueManagement.Domain.Types.Issue.IssueCategory (IssueCategory)
import Kernel.Types.Id

data IssueOption = IssueOption
  { id :: Id IssueOption,
    issueCategoryId :: Maybe (Id IssueCategory),
    merchantOperatingCityId :: Id MerchantOperatingCity,
    option :: Text,
    priority :: Int,
    issueMessageId :: Maybe Text,
    restrictedVariants :: [Common.VehicleVariant],
    restrictedRideStatuses :: [Common.RideStatus],
    showOnlyWhenUserBlocked :: Bool,
    label :: Maybe Text,
    merchantId :: Id Common.Merchant,
    isActive :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    igmSubCategory :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
