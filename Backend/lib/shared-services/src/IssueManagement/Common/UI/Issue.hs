{-# LANGUAGE DerivingVia #-}

module IssueManagement.Common.UI.Issue
  ( module IssueManagement.Common.UI.Issue,
    module Reexport,
  )
where

import AWS.S3 (FileType (..))
import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T hiding (count, map)
import EulerHS.Prelude hiding (id)
import IssueManagement.Common as Reexport hiding (Audio, Image)
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import IssueManagement.Domain.Types.MediaFile
import Kernel.External.Types (Language)
import Kernel.ServantMultipart
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Text.Read as TR (read)

type IssueCreateAPI =
  QueryParam "language" Language
    :> ReqBody '[JSON] IssueReportReq
    :> Post '[JSON] IssueReportRes

data IssueReportReq = IssueReportReq
  { rideId :: Maybe (Id Ride),
    mediaFiles :: [Id MediaFile],
    optionId :: Maybe (Id IssueOption),
    categoryId :: Id IssueCategory,
    description :: Text,
    chats :: Maybe [Chat],
    createTicket :: Maybe Bool,
    ticketBookingId :: Maybe (Id FRFSTicketBooking)
  }
  deriving (Generic, FromJSON, ToSchema, Show)

data IssueReportRes = IssueReportRes
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    messages :: [Message]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueListAPI =
  QueryParam "language" Language
    :> Get '[JSON] IssueReportListRes

newtype IssueReportListRes = IssueReportListRes
  { issues :: [IssueReportListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportListItem = IssueReportListItem
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    status :: IssueStatus,
    category :: Text,
    optionLabel :: Maybe Text,
    rideId :: Maybe (Id Ride),
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueInfoAPI =
  QueryParam "language" Language
    :> Get '[JSON] IssueInfoRes

data IssueInfoRes = IssueInfoRes
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    categoryLabel :: Maybe Text,
    option :: Maybe Text,
    assignee :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    mediaFiles :: [MediaFile_],
    createdAt :: UTCTime,
    chats :: [ChatDetail],
    options :: [IssueOptionRes],
    categoryId :: Maybe (Id IssueCategory)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFile_ = MediaFile_
  { _type :: FileType,
    url :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueUploadAPI =
  MultipartForm Tmp IssueMediaUploadReq
    :> Post '[JSON] IssueMediaUploadRes

data IssueMediaUploadReq = IssueMediaUploadReq
  { file :: FilePath,
    reqContentType :: Text,
    fileType :: FileType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp IssueMediaUploadReq where
  fromMultipart form = do
    IssueMediaUploadReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> fmap (TR.read . T.unpack) (lookupInput "fileType" form)

instance ToMultipart Tmp IssueMediaUploadReq where
  toMultipart issueMediaUploadReq =
    MultipartData
      [Input "fileType" (show issueMediaUploadReq.fileType)]
      [FileData "file" (T.pack issueMediaUploadReq.file) "" (issueMediaUploadReq.file)]

newtype IssueMediaUploadRes = IssueMediaUploadRes
  { fileId :: Id MediaFile
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueFetchMediaAPI =
  MandatoryQueryParam "filePath" Text
    :> Get '[JSON] Text

-------------------------------------------------------------------------

type IssueDeleteAPI =
  Delete '[JSON] APISuccess

-------------------------------------------------------------------------

type IssueUpdateAPI =
  ReqBody '[JSON] IssueUpdateReq
    :> Put '[JSON] APISuccess

data IssueUpdateReq = IssueUpdateReq
  { categoryId :: Id IssueCategory,
    optionId :: Id IssueOption
  }
  deriving (Generic, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueCategoryAPI =
  QueryParam "language" Language
    :> Get '[JSON] IssueCategoryListRes

data IssueCategoryRes = IssueCategoryRes
  { issueCategoryId :: Id IssueCategory,
    label :: Text,
    category :: Text,
    logoUrl :: Text,
    categoryType :: CategoryType,
    isRideRequired :: Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus]
  }
  deriving (Generic, Show, ToJSON, ToSchema)

newtype IssueCategoryListRes = IssueCategoryListRes
  { categories :: [IssueCategoryRes]
  }
  deriving (Generic, Show, ToJSON, ToSchema)

-------------------------------------------------------------------------

type IssueOptionAPI =
  MandatoryQueryParam "categoryId" (Id IssueCategory)
    :> QueryParam "optionId" (Id IssueOption)
    :> QueryParam "issueReportId" (Id IssueReport)
    :> QueryParam "rideId" (Id Ride)
    :> QueryParam "language" Language
    :> Get '[JSON] IssueOptionListRes

data IssueOptionRes = IssueOptionRes
  { issueOptionId :: Id IssueOption,
    label :: Text,
    option :: Text,
    mandatoryUploads :: Maybe [MandatoryUploads]
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

data Message = Message
  { id :: Id IssueMessage,
    message :: Text,
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    mediaFileUrls :: [Text],
    referenceCategoryId :: Maybe (Id IssueCategory),
    referenceOptionId :: Maybe (Id IssueOption),
    label :: Text
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

data IssueOptionListRes = IssueOptionListRes
  { options :: [IssueOptionRes],
    messages :: [Message]
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

-------------------------------------------------------------------------

type IssueStatusUpdateAPI =
  QueryParam "language" Language
    :> ReqBody '[JSON] IssueStatusUpdateReq
    :> Put '[JSON] IssueStatusUpdateRes

data IssueStatusUpdateReq = IssueStatusUpdateReq
  { status :: IssueStatus,
    customerResponse :: Maybe CustomerResponse,
    customerRating :: Maybe CustomerRating
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

newtype IssueStatusUpdateRes = IssueStatusUpdateRes
  { messages :: [Message]
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

-------------------------------------------------------------------------

type IgmStatusAPI =
  Post '[JSON] APISuccess

-------------------------------------------------------------------------

data CustomerResponse
  = ACCEPT
  | ESCALATE
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)

instance FromHttpApiData CustomerResponse where
  parseUrlPiece "accept" = pure ACCEPT
  parseUrlPiece "escalate" = pure ESCALATE
  parseUrlPiece _ = Left "Unable to parse customer response"

instance ToHttpApiData CustomerResponse where
  toUrlPiece ACCEPT = "accept"
  toUrlPiece ESCALATE = "escalate"

data CustomerRating
  = THUMBS_UP
  | THUMBS_DOWN
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)

instance FromHttpApiData CustomerRating where
  parseUrlPiece "thumbsup" = pure THUMBS_UP
  parseUrlPiece "thumbsdown" = pure THUMBS_DOWN
  parseUrlPiece _ = Left "Unable to parse customer rating"

instance ToHttpApiData CustomerRating where
  toUrlPiece THUMBS_UP = "thumbsup"
  toUrlPiece THUMBS_DOWN = "thumbsdown"
