{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Domain.Types.Issue.IGMIssue where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.Beam.Lib.UtilsTH
import Kernel.Types.Id
import Kernel.Utils.Common

data IGMIssue = IGMIssue
  { bookingId :: Text,
    createdAt :: UTCTime,
    customerEmail :: Maybe Text,
    customerName :: Maybe Text,
    customerPhone :: Maybe Text,
    id :: Id IGMIssue,
    issueRaisedByMerchant :: Text,
    issueStatus :: Status,
    issueType :: IssueType,
    merchantId :: Id Merchant,
    resolutionAction :: Maybe Text,
    respondentAction :: Maybe Text,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data IssueType = GRIEVANCE | ISSUE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Status = OPEN | CLOSED | ESCALATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''IssueType)

$(mkBeamInstancesForEnumAndList ''Status)
