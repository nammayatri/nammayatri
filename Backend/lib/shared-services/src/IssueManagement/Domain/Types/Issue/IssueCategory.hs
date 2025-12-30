module IssueManagement.Domain.Types.Issue.IssueCategory where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.Beam.Lib.UtilsTH
import Kernel.Types.Id
import Kernel.Utils.Common

data IssueCategory = IssueCategory
  { id :: Id IssueCategory,
    category :: Text,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    logoUrl :: Text,
    priority :: Int,
    merchantId :: Id Merchant,
    categoryType :: CategoryType,
    isActive :: Bool,
    isRideRequired :: Bool,
    isTicketRequired :: Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus],
    label :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    igmCategory :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data CategoryType = Category | FAQ
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

$(mkBeamInstancesForEnum ''CategoryType)
