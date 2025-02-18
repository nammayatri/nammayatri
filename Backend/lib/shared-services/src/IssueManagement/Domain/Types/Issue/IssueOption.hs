{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Domain.Types.Issue.IssueOption where

import Data.Time
import EulerHS.Prelude hiding (id)
import qualified IGM.Enums as Spec
import IssueManagement.Common
import qualified IssueManagement.Common as Common
import IssueManagement.Domain.Types.Issue.IssueCategory (IssueCategory)
import Kernel.Prelude hiding (Generic)
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
    igmSubCategory :: Maybe Spec.IssueSubCategory,
    mandatoryUploads :: Maybe [Common.MandatoryUploads]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
