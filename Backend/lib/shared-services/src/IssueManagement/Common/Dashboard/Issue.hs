{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Common.Dashboard.Issue
  ( module IssueManagement.Common.Dashboard.Issue,
    module Reexport,
  )
where

import Data.Aeson
import Data.Text as T hiding (count)
import Data.Time
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import IssueManagement.Domain.Types.MediaFile (MediaFile)
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (count)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.CacheFlow as Reexport
import Kernel.Types.Common
import Kernel.Types.HideSecrets as Reexport
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data IssueEndpoint
  = IssueUpdateEndpoint
  | IssueAddCommentEndpoint
  | TicketStatusCallBackEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "IssueEndpoint"

---------------------------------------------------------
-- issue category list --------------------------------

type IssueCategoryListAPI =
  "category"
    :> Get '[JSON] IssueCategoryListRes

data IssueCategoryRes = IssueCategoryRes
  { issueCategoryId :: Id IssueCategory,
    label :: Text,
    category :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype IssueCategoryListRes = IssueCategoryListRes
  { categories :: [IssueCategoryRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

---------------------------------------------------------
-- issues list --------------------------------------

type IssueListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "status" IssueStatus
    :> QueryParam "category" (Id IssueCategory)
    :> QueryParam "assignee" Text
    :> Get '[JSON] IssueReportListResponse

data IssueReportListResponse = IssueReportListResponse
  { issues :: [IssueReportListItem],
    summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportListItem = IssueReportListItem
  { issueReportId :: Id IssueReport,
    personId :: Id Person,
    rideId :: Maybe (Id Ride),
    deleted :: Bool,
    category :: Text,
    assignee :: Maybe Text,
    status :: IssueStatus,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- issue info --------------------------------

type IssueInfoAPI =
  Capture "issueId" (Id IssueReport)
    :> "info"
    :> Get '[JSON] IssueInfoRes

data IssueInfoRes = IssueInfoRes
  { issueReportId :: Id IssueReport,
    personDetail :: Maybe PersonDetail,
    rideId :: Maybe (Id Ride),
    comments :: [IssueReportCommentItem],
    category :: Text,
    mediaFiles :: [MediaFile],
    option :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    assignee :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonDetail = PersonDetail
  { personId :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportCommentItem = IssueReportCommentItem
  { comment :: Text,
    authorDetail :: AuthorDetail,
    timestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuthorDetail = AuthorDetail
  { authorId :: Id User,
    firstName :: Maybe Text,
    lastName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- update issue --------------------------------------

type IssueUpdateAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "update"
           :> ReqBody '[JSON] IssueUpdateReq
           :> Put '[JSON] APISuccess
       )

data IssueUpdateReq = IssueUpdateReq
  { status :: Maybe IssueStatus,
    assignee :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueUpdateReq where
  hideSecrets = identity

type IssueUpdateByUserAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "update"
           :> ReqBody '[JSON] IssueUpdateByUserReq
           :> Put '[JSON] APISuccess
       )

data IssueUpdateByUserReq = IssueUpdateByUserReq
  { status :: Maybe IssueStatus,
    assignee :: Maybe Text,
    userId :: Id User
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueUpdateByUserReq where
  hideSecrets = identity

---------------------------------------------------------
-- add comment --------------------------------------

type IssueAddCommentAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "comment"
           :> ReqBody '[JSON] IssueAddCommentReq
           :> Post '[JSON] APISuccess
       )

newtype IssueAddCommentReq = IssueAddCommentReq
  { comment :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentReq where
  hideSecrets = identity

type IssueAddCommentByUserAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "comment"
           :> ReqBody '[JSON] IssueAddCommentByUserReq
           :> Post '[JSON] APISuccess
       )

data IssueAddCommentByUserReq = IssueAddCommentByUserReq
  { comment :: Text,
    userId :: Id User
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentByUserReq where
  hideSecrets = identity

type IssueFetchMediaAPI =
  "media"
    :> MandatoryQueryParam "filePath" Text
    :> Get '[JSON] Text

---------------------------------------------------------
-- Ticket Status Call Back --------------------------------------

type TicketStatusCallBackAPI =
  "kapture"
    :> "ticketStatus"
    :> ReqBody '[JSON] TicketStatusCallBackReq
    :> Put '[JSON] APISuccess

data TicketStatusCallBackReq = TicketStatusCallBackReq
  { ticketId :: Text,
    status :: IssueStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets TicketStatusCallBackReq where
  hideSecrets = identity

data Summary = Summary
  { totalCount :: Int, --TODO add db indexes
    count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
