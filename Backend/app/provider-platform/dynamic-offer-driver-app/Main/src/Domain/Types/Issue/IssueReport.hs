{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Issue.IssueReport where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Issue.IssueCategory as D
import qualified Domain.Types.Issue.IssueOption as D
import qualified Domain.Types.MediaFile as D
import qualified Domain.Types.Person as D
import qualified Domain.Types.Ride as D
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common

data IssueStatus
  = NEW
  | INPROGRESS
  | RESOLVED
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON)

data IssueReport = IssueReport
  { id :: Id IssueReport,
    driverId :: Id D.Person,
    rideId :: Maybe (Id D.Ride),
    description :: Text,
    assignee :: Maybe Text,
    status :: IssueStatus,
    categoryId :: Id D.IssueCategory,
    optionId :: Maybe (Id D.IssueOption),
    deleted :: Bool,
    mediaFiles :: [Id D.MediaFile],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
