{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Domain.Types.Issue.IssueOption where

import Data.Time
import EulerHS.Prelude hiding (id)
import qualified IGM.Enums as Spec
import IssueManagement.Common
import qualified IssueManagement.Common as Common
import IssueManagement.Domain.Types.Issue.Common
import IssueManagement.Domain.Types.Issue.IssueCategory (IssueCategory)
import Kernel.Beam.Lib.UtilsTH
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
    allowedAttachements :: [AllowedAttachment],
    uiAction :: Maybe UIAction,
    onInputAction :: Maybe OnInputAction,
    filterTags :: [FilterOptionTags]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

-- What action the UI is supposed to perform
data UIAction = GetVpa | GetAmount | GetPlainText | GetFeedback
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

-- What logic to perform on the input from UI
data OnInputAction = ProcessRefund | ProcessDriverDemandedMore
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

type Description = Text

data AllowedAttachment = Image Description | Video Description | Audio Description
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

$(mkBeamInstancesForEnum ''OnInputAction)
$(mkBeamInstancesForEnum ''UIAction)
$(mkBeamInstancesForEnumAndList ''AllowedAttachment)
