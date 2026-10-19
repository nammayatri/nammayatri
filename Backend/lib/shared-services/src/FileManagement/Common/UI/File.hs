module FileManagement.Common.UI.File where

import AWS.S3 (FileType (..))
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.MediaFile (MediaFile, MediaFileUploadStatus)
import Kernel.ServantMultipart
import Kernel.Types.Id
import qualified Text.Read as TR (readMaybe)

data FileDomain = RideChat | RideEvidence
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DomainConfig = DomainConfig
  { s3Prefix :: Text,
    routeSegment :: Maybe Text,
    scopeToUploader :: Bool,
    storeRawBytes :: Bool
  }
  deriving stock (Eq, Show, Generic)

domainConfig :: FileDomain -> DomainConfig
domainConfig RideChat =
  DomainConfig
    { s3Prefix = "ride-chat",
      routeSegment = Just "files",
      scopeToUploader = False,
      storeRawBytes = False
    }
domainConfig RideEvidence =
  DomainConfig
    { s3Prefix = "ride-evidence",
      routeSegment = Nothing,
      scopeToUploader = True,
      storeRawBytes = True
    }

-- Where the files of one uploader live in a domain that scopes its paths to the uploader.
ownerFolder :: FileDomain -> Text -> Text
ownerFolder fileDomain ownerId = (domainConfig fileDomain).s3Prefix <> "/" <> ownerId <> "/"

-- The part of a stored path that starts at the folder of a domain, or empty when the path is not in that folder. The S3 path prefix of the environment, and any leading slash, come before it.
fromDomainFolder :: FileDomain -> Text -> Text
fromDomainFolder fileDomain = snd . T.breakOn ((domainConfig fileDomain).s3Prefix <> "/")

isInDomainFolder :: FileDomain -> Text -> Bool
isInDomainFolder fileDomain = not . T.null . fromDomainFolder fileDomain

isOwnedBy :: FileDomain -> Text -> Text -> Bool
isOwnedBy fileDomain ownerId path = ownerFolder fileDomain ownerId `T.isPrefixOf` fromDomainFolder fileDomain path

-- A key the server wrote holds only these characters, so a path with anything else is not one of ours.
isPlainKey :: Text -> Bool
isPlainKey = T.all (\c -> (isAscii c && isAlphaNum c) || c `elem` ['.', '_', '/', '-'])

-- A file in an uploader-scoped domain can only be read by the person who uploaded it. Every other path is not restricted here.
canReadFile :: Text -> Text -> Bool
canReadFile readerId path
  | isInDomainFolder RideEvidence path = isOwnedBy RideEvidence readerId path && isPlainKey path && not (".." `T.isInfixOf` path)
  | otherwise = True

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

-- The first bytes of the image formats a proof may use: png, jpeg, gif, webp and heic or heif.
-- A file that does not start like one of them, an empty file included, is not accepted as an image.
looksLikeImage :: BS.ByteString -> Bool
looksLikeImage bytes =
  any (`BS.isPrefixOf` bytes) [BS.pack [0x89, 0x50, 0x4E, 0x47], BS.pack [0xFF, 0xD8, 0xFF], "GIF87a", "GIF89a"]
    || (BS.take 4 bytes == "RIFF" && BS.take 4 (BS.drop 8 bytes) == "WEBP")
    || BS.take 4 (BS.drop 4 bytes) == "ftyp"
