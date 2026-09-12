{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Message where

import qualified AWS.S3
import qualified Dashboard.Common
import Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data AddLinkAsMedia = AddLinkAsMedia {url :: Kernel.Prelude.Text, fileType :: AWS.S3.FileType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddMessageRequest = AddMessageRequest
  { _type :: MessageType,
    title :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alwaysTriggerOnOnboarding :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    translations :: [MessageTranslation],
    mediaFiles :: [Kernel.Types.Id.Id Dashboard.Common.File]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AddMessageRequest where
  hideSecrets = Kernel.Prelude.identity

newtype AddMessageResponse = AddMessageResponse {messageId :: Kernel.Types.Id.Id Dashboard.Common.Message}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EditMessageRequest = EditMessageRequest
  { messageId :: Kernel.Types.Id.Id Dashboard.Common.Message,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    messageTranslations :: [MessageTranslation]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InputType
  = Include
  | Exclude
  | AllEnabled
  deriving stock (Eq, Show, Generic, Kernel.Prelude.Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFile = MediaFile {_type :: AWS.S3.FileType, link :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageDeliveryInfoResponse = MessageDeliveryInfoResponse
  { messageId :: Kernel.Types.Id.Id Dashboard.Common.Message,
    success :: Kernel.Prelude.Int,
    failed :: Kernel.Prelude.Int,
    queued :: Kernel.Prelude.Int,
    sending :: Kernel.Prelude.Int,
    seen :: Kernel.Prelude.Int,
    liked :: Kernel.Prelude.Int,
    viewed :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageDeliveryStatus
  = Failed
  | Success
  | Queued
  | Sending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data MessageInfoResponse = MessageInfoResponse
  { messageId :: Kernel.Types.Id.Id Dashboard.Common.Message,
    title :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Text,
    _type :: MessageType,
    mediaFiles :: [MediaFile],
    shareable :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageListItem = MessageListItem {messageId :: Kernel.Types.Id.Id Dashboard.Common.Message, title :: Kernel.Prelude.Text, _type :: MessageType, shareable :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageListResponse = MessageListResponse {messages :: [MessageListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageReceiverListItem = MessageReceiverListItem
  { receiverId :: Kernel.Types.Id.Id Dashboard.Common.Receiver,
    receiverName :: Kernel.Prelude.Text,
    receiverNumber :: Kernel.Prelude.Text,
    reply :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    seen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    liked :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    status :: MessageDeliveryStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageReceiverListResponse = MessageReceiverListResponse {receivers :: [MessageReceiverListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageTranslation = MessageTranslation
  { language :: Kernel.External.Types.Language,
    title :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageType
  = Action Kernel.Prelude.Text
  | Read
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SendMessageRequest = SendMessageRequest {csvFile :: Kernel.Prelude.Maybe Kernel.Prelude.FilePath, _type :: InputType, messageId :: Kernel.Prelude.Text, scheduledTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SendMessageRequest where
  hideSecrets = Kernel.Prelude.identity

data UploadFileRequest = UploadFileRequest {file :: Kernel.Prelude.FilePath, reqContentType :: Kernel.Prelude.Text, fileType :: AWS.S3.FileType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UploadFileRequest where
  hideSecrets = Kernel.Prelude.identity

newtype UploadFileResponse = UploadFileResponse {fileId :: Kernel.Types.Id.Id Dashboard.Common.File}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("message" :> (PostMessageUploadFile :<|> PostMessageAddLink :<|> PostMessageAdd :<|> PostMessageSend :<|> PostMessageEdit :<|> GetMessageList :<|> GetMessageInfo :<|> GetMessageDeliveryInfo :<|> GetMessageReceiverList))

type PostMessageUploadFile = ("uploadFile" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp UploadFileRequest :> Post '[JSON] UploadFileResponse)

type PostMessageAddLink = ("addLink" :> ReqBody '[JSON] AddLinkAsMedia :> Post '[JSON] UploadFileResponse)

type PostMessageAdd = ("add" :> ReqBody '[JSON] AddMessageRequest :> Post '[JSON] AddMessageResponse)

type PostMessageSend = ("send" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp SendMessageRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMessageEdit = ("edit" :> ReqBody '[JSON] EditMessageRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMessageList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] MessageListResponse)

type GetMessageInfo = (Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "info" :> Get '[JSON] MessageInfoResponse)

type GetMessageDeliveryInfo = (Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "deliveryInfo" :> Get '[JSON] MessageDeliveryInfoResponse)

type GetMessageReceiverList =
  ( Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "receiverList" :> QueryParam "number" Kernel.Prelude.Text
      :> QueryParam
           "status"
           MessageDeliveryStatus
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           MessageReceiverListResponse
  )

data MessageAPIs = MessageAPIs
  { postMessageUploadFile :: (Data.ByteString.Lazy.ByteString, UploadFileRequest) -> EulerHS.Types.EulerClient UploadFileResponse,
    postMessageAddLink :: AddLinkAsMedia -> EulerHS.Types.EulerClient UploadFileResponse,
    postMessageAdd :: AddMessageRequest -> EulerHS.Types.EulerClient AddMessageResponse,
    postMessageSend :: (Data.ByteString.Lazy.ByteString, SendMessageRequest) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMessageEdit :: EditMessageRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMessageList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient MessageListResponse,
    getMessageInfo :: Kernel.Types.Id.Id Dashboard.Common.Message -> EulerHS.Types.EulerClient MessageInfoResponse,
    getMessageDeliveryInfo :: Kernel.Types.Id.Id Dashboard.Common.Message -> EulerHS.Types.EulerClient MessageDeliveryInfoResponse,
    getMessageReceiverList :: Kernel.Types.Id.Id Dashboard.Common.Message -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe MessageDeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient MessageReceiverListResponse
  }

mkMessageAPIs :: (Client EulerHS.Types.EulerClient API -> MessageAPIs)
mkMessageAPIs messageClient = (MessageAPIs {..})
  where
    postMessageUploadFile :<|> postMessageAddLink :<|> postMessageAdd :<|> postMessageSend :<|> postMessageEdit :<|> getMessageList :<|> getMessageInfo :<|> getMessageDeliveryInfo :<|> getMessageReceiverList = messageClient

data MessageUserActionType
  = POST_MESSAGE_UPLOAD_FILE
  | POST_MESSAGE_ADD_LINK
  | POST_MESSAGE_ADD
  | POST_MESSAGE_SEND
  | POST_MESSAGE_EDIT
  | GET_MESSAGE_LIST
  | GET_MESSAGE_INFO
  | GET_MESSAGE_DELIVERY_INFO
  | GET_MESSAGE_RECEIVER_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''MessageDeliveryStatus)

$(Data.Singletons.TH.genSingletons [''MessageUserActionType])
