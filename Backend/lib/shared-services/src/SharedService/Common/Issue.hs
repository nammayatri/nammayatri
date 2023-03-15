{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module SharedService.Common.Issue
  ( module SharedService.Common.Issue,
    module Reexport,
  )
where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text as T hiding (map)
import Data.Text.Encoding as DT
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language)
import Kernel.ServantMultipart
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedService.Common as Reexport

type IssueCreateAPI =
  ReqBody '[JSON] IssueReportReq
    :> Post '[JSON] IssueReportRes

data IssueReportReq = IssueReportReq
  { rideId :: Maybe (Id Ride),
    mediaFiles :: [Id MediaFile],
    option :: Maybe Text,
    category :: Text,
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
  Get '[JSON] IssueReportDriverListRes

newtype IssueReportDriverListRes = IssueReportDriverListRes
  { issues :: [IssueReportDriverListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportDriverListItem = IssueReportDriverListItem
  { issueReportId :: Id IssueReport,
    category :: Text,
    option :: Maybe Text,
    assignee :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    mediaFiles :: [MediaFile_]
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

newtype IssueUpdateReq = IssueUpdateReq
  { option :: Maybe Text
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
