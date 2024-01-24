{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Message
  ( module Dashboard.Common.Message,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import Data.OpenApi hiding (description, name, password, summary, title, url)
import Data.Text as T
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
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
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

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

data FileType = Audio | Video | Image | AudioLink | VideoLink | ImageLink | PortraitVideoLink
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
    shortDescription :: Text,
    label :: Maybe Text,
    alwaysTriggerOnOnboarding :: Maybe Bool,
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
    description :: Text,
    shortDescription :: Text,
    label :: Maybe Text
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
  { csvFile :: Maybe FilePath,
    _type :: InputType,
    messageId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SendMessageRequest where
  fromMultipart form = do
    let inputType = fmap (read . T.unpack) (lookupInput "type" form)
    SendMessageRequest
      <$> either (helper inputType) (Right . Just . fdPayload) (lookupFile "csvFile" form)
      <*> inputType
      <*> lookupInput "messageId" form
    where
      helper (Right AllEnabled) _ = Right Nothing
      helper _ x = Left x

instance ToMultipart Tmp SendMessageRequest where
  toMultipart form =
    MultipartData
      [ Input "type" $ show form._type,
        Input "messageId" form.messageId
      ]
      (maybe [] (\file -> [FileData "csvFile" (T.pack file) "text/csv" file]) form.csvFile)

data InputType = Include | Exclude | AllEnabled
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
  Capture "messageId" (Id Message)
    :> "info"
    :> Get '[JSON] MessageInfoResponse

data MessageInfoResponse = MessageInfoResponse
  { messageId :: Id Message,
    title :: Text,
    description :: Text,
    shortDescription :: Text,
    _type :: MessageType,
    mediaFiles :: [MediaFile]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---
-- Message Delivery Info
--
type MessageDeliveryInfoAPI =
  Capture "messageId" (Id Message)
    :> "deliveryInfo"
    :> Get '[JSON] MessageDeliveryInfoResponse

data MessageDeliveryInfoResponse = MessageDeliveryInfoResponse
  { messageId :: Id Message,
    success :: Int,
    failed :: Int,
    queued :: Int,
    sending :: Int,
    seen :: Int,
    liked :: Int,
    viewed :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---
-- MessageReceiverList
--
type MessageReceiverListAPI =
  Capture "messageId" (Id Message)
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
    liked :: Maybe Bool,
    status :: MessageDeliveryStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageDeliveryStatus = Failed | Success | Queued | Sending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''MessageDeliveryStatus)

-- validateUpdatePhoneNumberReq :: Validate UpdatePhoneNumberReq
-- validateUpdatePhoneNumberReq UpdatePhoneNumberReq {..} =
--   sequenceA_
--     [ validateField "newPhoneNumber" newPhoneNumber P.mobileNumber,
--       validateField "newCountryCode" newCountryCode P.mobileCountryCode
--     ]
