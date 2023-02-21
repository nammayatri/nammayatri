{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Message
  ( module Dashboard.Common.Message,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.Bifunctor as BF
import Data.ByteString.Lazy as BSL
import Data.OpenApi hiding (description, name, password, summary, title, url)
import Data.Text as T
import Data.Text.Encoding as DT
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data MessageEndpoint
  = UploadFileEndpoint
  | AddLinkEndpoint
  | AddMessageEndpoint
  | SendMessageEndpoint
  | MessageListEndpoint
  | MessageInfoEndpoint
  | MessageDeliveryInfoEndpoint
  | MessageReceiverListEndpoint
  deriving (Show, Read)

derivePersistField "MessageEndpoint"

---
-- Upload File
--
type UploadFileAPI =
  "uploadFile"
    :> MultipartForm Tmp UploadFileRequest
    :> Post '[JSON] UploadFileResponse

type AddLinkAPI =
  "addLink"
    :> ReqBody '[JSON] AddLinkAsMedia
    :> Post '[JSON] UploadFileResponse

data AddLinkAsMedia = AddLinkAsMedia
  { url :: Text,
    fileType :: FileType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadFileRequest = UploadFileRequest
  { file :: FilePath,
    reqContentType :: Text,
    fileType :: FileType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UploadFileRequest where
  fromMultipart form = do
    UploadFileRequest
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> fmap (read . T.unpack) (lookupInput "fileType" form)

instance ToMultipart Tmp UploadFileRequest where
  toMultipart uploadFileRequest =
    MultipartData
      [Input "fileType" (show uploadFileRequest.fileType)]
      [FileData "file" (T.pack uploadFileRequest.file) "" (uploadFileRequest.file)]

data FileType = Audio | Video | Image | AudioLink | VideoLink | ImageLink
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UploadFileResponse = UploadFileResponse
  { fileId :: Id File
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UploadFileRequest where
  hideSecrets = identity

---
-- Add Message
--
type AddMessageAPI =
  "add"
    :> ReqBody '[JSON] AddMessageRequest
    :> Post '[JSON] AddMessageResponse

data AddMessageRequest = AddMessageRequest
  { _type :: MessageType, -- (Action Text | Read)
    title :: Text, -- max character 100
    description :: Text, -- no max character limit
    translations :: [MessageTranslation],
    mediaFiles :: [Id File]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageType = Action Text | Read
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFile = MediaFile
  { _type :: FileType,
    link :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype AddMessageResponse = AddMessageResponse
  { messageId :: Id Message
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets AddMessageRequest where
  hideSecrets = identity

---
-- Send Message
--
type SendMessageAPI =
  "send"
    :> MultipartForm Tmp SendMessageRequest
    :> Post '[JSON] APISuccess

data SendMessageRequest = SendMessageRequest
  { csvFile :: FilePath,
    _type :: CSVType,
    messageId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SendMessageRequest where
  fromMultipart form = do
    SendMessageRequest
      <$> fmap fdPayload (lookupFile "csvFile" form)
      <*> fmap (read . T.unpack) (lookupInput "type" form)
      <*> lookupInput "messageId" form

instance ToMultipart Tmp SendMessageRequest where
  toMultipart form =
    MultipartData
      [ Input "type" (show form._type),
        Input "messageId" form.messageId
      ]
      [FileData "csvFile" (T.pack form.csvFile) "text/csv" form.csvFile]

data CSVType = Include | Exclude
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets SendMessageRequest where
  hideSecrets = identity

---
-- Message List
--
type MessageListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] MessageListResponse

data MessageListResponse = MessageListResponse
  { messages :: [MessageListItem],
    summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageListItem = MessageListItem
  { messageId :: Id Message,
    title :: Text,
    _type :: MessageType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---
-- Message Info
--
type MessageInfoAPI =
  "message"
    :> Capture "messageId" (Id Message)
    :> "info"
    :> Get '[JSON] MessageInfoResponse

data MessageInfoResponse = MessageInfoResponse
  { messageId :: Id Message,
    title :: Text,
    description :: Text,
    _type :: MessageType,
    mediaFiles :: [MediaFile]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---
-- Message Delivery Info
--
type MessageDeliveryInfoAPI =
  "message"
    :> Capture "messageId" (Id Message)
    :> "deliveryInfo"
    :> Get '[JSON] MessageDeliveryInfoResponse

data MessageDeliveryInfoResponse = MessageDeliveryInfoResponse
  { messageId :: Id Message,
    success :: Int,
    failed :: Int,
    pending :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---
-- MessageReceiverList
--
type MessageReceiverListAPI =
  "message"
    :> Capture "messageId" (Id Message)
    :> "receiverList"
    :> QueryParam "number" Text
    :> QueryParam "status" MessageDeliveryStatus
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] MessageReceiverListResponse

data MessageReceiverListResponse = MessageReceiverListResponse
  { receivers :: [MessageReceiverListItem],
    summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageReceiverListItem = MessageReceiverListItem
  { receiverId :: Id Receiver,
    receiverName :: Text,
    receiverNumber :: Text,
    reply :: Maybe Text,
    seen :: Maybe Bool,
    status :: MessageDeliveryStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageDeliveryStatus = Failed | Success | Pending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData MessageDeliveryStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData MessageDeliveryStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

-- validateUpdatePhoneNumberReq :: Validate UpdatePhoneNumberReq
-- validateUpdatePhoneNumberReq UpdatePhoneNumberReq {..} =
--   sequenceA_
--     [ validateField "newPhoneNumber" newPhoneNumber P.mobileNumber,
--       validateField "newCountryCode" newCountryCode P.mobileIndianCode
--     ]
