{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Domain.Types.Issue.IGMIssueAction where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as IGM
import Kernel.Beam.Lib.UtilsTH
import Kernel.Types.Id
import Kernel.Utils.Common

data ActionType = COMPLAINANT | RESPONDENT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''ActionType)

data IGMIssueAction = IGMIssueAction
  { id :: Id IGMIssueAction,
    igmIssueId :: Id IGM.IGMIssue,
    actionType :: ActionType,
    action :: Text,
    shortDesc :: Maybe Text,
    updatedAt :: UTCTime,
    updatedByOrgName :: Maybe Text,
    updatedByContactPhone :: Maybe Text,
    updatedByContactEmail :: Maybe Text,
    updatedByPersonName :: Maybe Text,
    cascadedLevel :: Maybe Int,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
