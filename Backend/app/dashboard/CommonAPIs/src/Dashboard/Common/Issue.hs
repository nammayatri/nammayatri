{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Issue
  ( module Dashboard.Common.Issue,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.Bifunctor as BF
import Data.ByteString.Lazy as BSL
import Data.Text as T
import Data.Text.Encoding as DT
import Data.Time
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data IssueEndpoint
  = IssueUpdateEndpoint
  | IssueAddCommentEndpoint
  deriving (Show, Read)

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
    driverId :: Id Driver,
    rideId :: Maybe (Id Ride),
    category :: Text,
    assignee :: Maybe Text,
    status :: IssueStatus,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueStatus = NEW | INPROGRESS | RESOLVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData IssueStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData IssueStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

---------------------------------------------------------
-- issue info --------------------------------

type IssueInfoAPI =
  Capture "issueId" (Id IssueReport)
    :> "info"
    :> Get '[JSON] IssueInfoRes

data IssueInfoRes = IssueInfoRes
  { issueReportId :: Id IssueReport,
    driverDetail :: Maybe DriverDetail,
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

data DriverDetail = DriverDetail
  { driverId :: Id Driver,
    firstName :: Text,
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

data MediaFile = MediaFile
  { _type :: FileType,
    url :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FileType = Audio | Image
  deriving stock (Eq, Show, Read, Generic)
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
