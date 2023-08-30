{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedService.Common.Issue
  ( module SharedService.Common.Issue,
    module Reexport,
  )
where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text as T hiding (map)
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language)
import Kernel.ServantMultipart
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant
import SharedService.Common as Reexport
import Text.Read (read)

type IssueCreateAPI =
  ReqBody '[JSON] IssueReportReq
    :> Post '[JSON] IssueReportRes

data IssueReportReq = IssueReportReq
  { rideId :: Maybe (Id Ride),
    mediaFiles :: [Id MediaFile],
    optionId :: Maybe (Id IssueOption),
    categoryId :: Id IssueCategory,
    description :: Text
  }
  deriving (Generic, FromJSON, ToSchema)

newtype IssueReportRes = IssueReportRes
  { issueReportId :: Id IssueReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------

type IssueListAPI =
  QueryParam "language" Language
    :> Get '[JSON] IssueReportDriverListRes

newtype IssueReportDriverListRes = IssueReportDriverListRes
  { issues :: [IssueReportDriverListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportDriverListItem = IssueReportDriverListItem
  { issueReportId :: Id IssueReport,
    status :: IssueStatus,
    category :: Text,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueStatus = OPEN | PENDING | RESOLVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''IssueStatus)

-------------------------------------------------------------------------

type IssueInfoAPI =
  QueryParam "language" Language
    :> Get '[JSON] IssueInfoRes

data IssueInfoRes = IssueInfoRes
  { issueReportId :: Id IssueReport,
    category :: Text,
    option :: Maybe Text,
    assignee :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    mediaFiles :: [MediaFile_],
    createdAt :: UTCTime
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
    :> QueryParam "language" Language
    :> Get '[JSON] IssueOptionListRes

data IssueOptionRes = IssueOptionRes
  { issueOptionId :: Id IssueOption,
    label :: Text,
    option :: Text
  }
  deriving (Generic, Show, ToJSON, ToSchema)

newtype IssueOptionListRes = IssueOptionListRes
  { options :: [IssueOptionRes]
  }
  deriving (Generic, Show, ToJSON, ToSchema)
