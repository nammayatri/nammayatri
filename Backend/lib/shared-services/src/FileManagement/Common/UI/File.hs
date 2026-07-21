module FileManagement.Common.UI.File where

import AWS.S3 (FileType (..))
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.MediaFile (MediaFile, MediaFileUploadStatus)
import Kernel.ServantMultipart
import Kernel.Types.Id
import qualified Text.Read as TR (readMaybe)

data FileDomain = RideChat
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DomainConfig = DomainConfig
  { s3Prefix :: Text,
    routeSegment :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

domainConfig :: FileDomain -> DomainConfig
domainConfig RideChat =
  DomainConfig
    { s3Prefix = "ride-chat",
      routeSegment = Just "files"
    }

data FileUploadReq = FileUploadReq
  { file :: FilePath,
    reqContentType :: Text,
    fileType :: FileType,
    domain :: FileDomain
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp FileUploadReq where
  fromMultipart form =
    FileUploadReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> parseInput "fileType" form
      <*> parseInput "domain" form

parseInput :: Read a => Text -> MultipartData Tmp -> Either String a
parseInput name form = do
  raw <- lookupInput name form
  case TR.readMaybe (T.unpack raw) of
    Just v -> Right v
    Nothing -> Left ("could not parse " <> T.unpack name <> ": " <> T.unpack raw)

data FileUploadRes = FileUploadRes
  { fileId :: Id MediaFile,
    filePath :: Text,
    url :: Maybe Text,
    status :: MediaFileUploadStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
