{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Issue.IssueReport where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as D
import qualified Domain.Types.Ride as D
import qualified Domain.Types.Message.MediaFile as D
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
    category :: Text,
    option :: Maybe Text,
    deleted :: Bool,
    mediaFiles :: [Id D.MediaFile],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
