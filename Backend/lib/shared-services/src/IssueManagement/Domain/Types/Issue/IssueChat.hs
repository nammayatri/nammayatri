module IssueManagement.Domain.Types.Issue.IssueChat where

import Data.Aeson
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common

data IssueChat = IssueChat
  { id :: Id IssueChat,
    ticketId :: Text,
    rideId :: Maybe (Id Ride),
    personId :: Id Person,
    chats :: [Text],
    mediaFiles :: [Text],
    issueReportId :: Maybe (Id IssueReport),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, BP.ToSchema)
