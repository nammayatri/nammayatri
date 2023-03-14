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
import Data.Time
import Data.Text as T
import Data.Text.Encoding as DT
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data IssueEndpoint
  = IssueListEndpoint
  | IssueUpdateEndpoint
  | IssueAddCommentEndpoint
  deriving (Show, Read)

derivePersistField "IssueEndpoint"

---------------------------------------------------------
-- issues list --------------------------------------

type IssueListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "status" IssueStatus
    :> QueryParam "category" Text
    :> QueryParam "assignee" Text
    :> Get '[JSON] IssueReportListResponse

data IssueReportListResponse = IssueReportListResponse
  { issues :: [IssueReportListItem]
  , summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportListItem = IssueReportListItem
  { issueReportId :: Id IssueReport,
    driverId :: Id Driver,
    rideId :: Maybe (Id Ride),
    category :: Text,
    option :: Maybe Text,
    assignee :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    comments :: [IssueReportCommentItem],
    mediaFiles :: [MediaFile]
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

data IssueReportCommentItem = IssueReportCommentItem
  { comment :: Text,
    author :: Text,
    timestamp :: UTCTime
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

---------------------------------------------------------
-- add comment --------------------------------------

type IssueAddCommentAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "comment"
          :> ReqBody '[JSON] IssueAddCommentReq
          :> Post '[JSON] APISuccess
       )

data IssueAddCommentReq = IssueAddCommentReq
  { comment :: Text,
    author :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentReq where
  hideSecrets = identity