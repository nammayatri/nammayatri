{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Common.UI.Issue
  ( module IssueManagement.Common.UI.Issue,
    module Reexport,
  )
where

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
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant hiding (Summary)
import SharedService.Common as Reexport
import Text.Read (read)

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
    chats :: [Chat]
  }
  deriving (Generic, FromJSON, ToSchema, Show)

data IssueReportRes = IssueReportRes
  { issueReportId :: Id IssueReport,
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
    status :: IssueStatus,
    category :: Text,
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
    categoryLabel :: Text,
    option :: Maybe Text,
    assignee :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    mediaFiles :: [MediaFile_],
    createdAt :: UTCTime,
    chats :: [ChatDetail],
    options :: [IssueOptionRes],
    categoryId :: Id IssueCategory
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFile_ = MediaFile_
  { _type :: FileType,
    url :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FileType = Audio | Image
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChatDetail = ChatDetail
  { timestamp :: UTCTime,
    content :: Maybe Text,
    id :: Text,
    chatType :: MessageType,
    sender :: Sender,
    label :: Maybe Text
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
      <*> fmap (read . T.unpack) (lookupInput "fileType" form)

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

data IssueMediaUploadConfig = IssueMediaUploadConfig
  { mediaFileSizeUpperLimit :: Int,
    mediaFileUrlPattern :: Text
  }

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
    logoUrl :: Text
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
    :> QueryParam "language" Language
    :> Get '[JSON] IssueOptionListRes

data IssueOptionRes = IssueOptionRes
  { issueOptionId :: Id IssueOption,
    label :: Text,
    option :: Text
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

data Message = Message
  { id :: Id IssueMessage,
    message :: Text
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

newtype IssueStatusUpdateReq = IssueStatusUpdateReq
  { status :: IssueStatus
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)

newtype IssueStatusUpdateRes = IssueStatusUpdateRes
  { messages :: [Message]
  }
  deriving (Generic, Show, ToJSON, ToSchema, Eq, FromJSON)
