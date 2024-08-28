{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Message where

import qualified AWS.S3
import qualified Dashboard.Common
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AddLinkAsMedia = AddLinkAsMedia {url :: Kernel.Prelude.Text, fileType :: AWS.S3.FileType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddMessageRequest = AddMessageRequest
  { _type :: API.Types.ProviderPlatform.Management.Message.MessageType,
    title :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alwaysTriggerOnOnboarding :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    translations :: [API.Types.ProviderPlatform.Management.Message.MessageTranslation],
    mediaFiles :: [Kernel.Types.Id.Id Dashboard.Common.File]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AddMessageRequest where
  hideSecrets = Kernel.Prelude.identity

newtype AddMessageResponse = AddMessageResponse {messageId :: Kernel.Types.Id.Id Dashboard.Common.Message}
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
    _type :: API.Types.ProviderPlatform.Management.Message.MessageType,
    mediaFiles :: [API.Types.ProviderPlatform.Management.Message.MediaFile]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageListItem = MessageListItem {messageId :: Kernel.Types.Id.Id Dashboard.Common.Message, title :: Kernel.Prelude.Text, _type :: API.Types.ProviderPlatform.Management.Message.MessageType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageListResponse = MessageListResponse {messages :: [API.Types.ProviderPlatform.Management.Message.MessageListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageReceiverListItem = MessageReceiverListItem
  { receiverId :: Kernel.Types.Id.Id Dashboard.Common.Receiver,
    receiverName :: Kernel.Prelude.Text,
    receiverNumber :: Kernel.Prelude.Text,
    reply :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    seen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    liked :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    status :: API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageReceiverListResponse = MessageReceiverListResponse {receivers :: [API.Types.ProviderPlatform.Management.Message.MessageReceiverListItem], summary :: Dashboard.Common.Summary}
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

data SendMessageRequest = SendMessageRequest {csvFile :: Kernel.Prelude.Maybe Kernel.Prelude.FilePath, _type :: API.Types.ProviderPlatform.Management.Message.InputType, messageId :: Kernel.Prelude.Text}
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

type API = ("message" :> (PostMessageUploadFile :<|> PostMessageAddLink :<|> PostMessageAdd :<|> PostMessageSend :<|> GetMessageList :<|> GetMessageInfo :<|> GetMessageDeliveryInfo :<|> GetMessageReceiverList))

type PostMessageUploadFile =
  ( "uploadFile" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp API.Types.ProviderPlatform.Management.Message.UploadFileRequest
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Message.UploadFileResponse
  )

type PostMessageAddLink =
  ( "addLink" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Message.UploadFileResponse
  )

type PostMessageAdd = ("add" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Message.AddMessageRequest :> Post '[JSON] API.Types.ProviderPlatform.Management.Message.AddMessageResponse)

type PostMessageSend =
  ( "send" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp API.Types.ProviderPlatform.Management.Message.SendMessageRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMessageList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] API.Types.ProviderPlatform.Management.Message.MessageListResponse)

type GetMessageInfo = (Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "info" :> Get '[JSON] API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)

type GetMessageDeliveryInfo =
  ( Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "deliveryInfo"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse
  )

type GetMessageReceiverList =
  ( Capture "messageId" (Kernel.Types.Id.Id Dashboard.Common.Message) :> "receiverList" :> QueryParam "number" Kernel.Prelude.Text
      :> QueryParam
           "status"
           API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse
  )

data MessageAPIs = MessageAPIs
  { postMessageUploadFile ::
      ( Data.ByteString.Lazy.ByteString,
        API.Types.ProviderPlatform.Management.Message.UploadFileRequest
      ) ->
      EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.UploadFileResponse,
    postMessageAddLink :: API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.UploadFileResponse,
    postMessageAdd :: API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.AddMessageResponse,
    postMessageSend :: (Data.ByteString.Lazy.ByteString, API.Types.ProviderPlatform.Management.Message.SendMessageRequest) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMessageList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageListResponse,
    getMessageInfo :: Kernel.Types.Id.Id Dashboard.Common.Message -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageInfoResponse,
    getMessageDeliveryInfo :: Kernel.Types.Id.Id Dashboard.Common.Message -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse,
    getMessageReceiverList :: Kernel.Types.Id.Id Dashboard.Common.Message -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse
  }

mkMessageAPIs :: (Client EulerHS.Types.EulerClient API -> MessageAPIs)
mkMessageAPIs messageClient = (MessageAPIs {..})
  where
    postMessageUploadFile :<|> postMessageAddLink :<|> postMessageAdd :<|> postMessageSend :<|> getMessageList :<|> getMessageInfo :<|> getMessageDeliveryInfo :<|> getMessageReceiverList = messageClient

data MessageEndpointDSL
  = PostMessageUploadFileEndpoint
  | PostMessageAddLinkEndpoint
  | PostMessageAddEndpoint
  | PostMessageSendEndpoint
  | GetMessageListEndpoint
  | GetMessageInfoEndpoint
  | GetMessageDeliveryInfoEndpoint
  | GetMessageReceiverListEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
