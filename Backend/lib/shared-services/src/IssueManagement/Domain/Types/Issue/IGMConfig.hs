module IssueManagement.Domain.Types.Issue.IGMConfig where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data IGMConfig = IGMConfig
  { expectedResolutionTime :: Int,
    expectedResponseTime :: Int,
    groEmail :: Text,
    groName :: Text,
    groPhone :: Text,
    respondentName :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentEmail :: Maybe Text,
    resolutionProviderName :: Maybe Text,
    resolutionProviderPhone :: Maybe Text,
    resolutionProviderEmail :: Maybe Text,
    id :: Id IGMConfig,
    merchantId :: Id Merchant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
