{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Domain.Types.Issue.IGMIssue where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import qualified IGM.Enums as Spec
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
    riderId :: Maybe (Id Person),
    issueRaisedByMerchant :: Maybe Text,
    issueStatus :: Status,
    issueType :: IssueType,
    merchantId :: Maybe (Id Merchant),
    respondentName :: Maybe Text,
    respondentEmail :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentAction :: Maybe Text,
    respondingMerchantId :: Maybe Text,
    respondentEntityType :: Maybe Text,
    transactionId :: Text,
    domain :: Spec.Domain,
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data IssueType = GRIEVANCE | ISSUE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Status = OPEN | CLOSED | ESCALATED | RESOLVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''IssueType)

$(mkBeamInstancesForEnumAndList ''Status)

$(mkBeamInstancesForEnumAndList ''Spec.Domain)

mkStringToDomainType :: Text -> Spec.Domain
mkStringToDomainType "ONDC:TRV10" = Spec.ON_DEMAND
mkStringToDomainType "ONDC:TRV11" = Spec.PUBLIC_TRANSPORT
mkStringToDomainType _ = error "Invalid Domain"
