{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Communication where

import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data CTAButtonReq = CTAButtonReq {label :: Kernel.Prelude.Text, url :: Kernel.Prelude.Text, linkType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationChannelType
  = CH_PUSH
  | CH_SMS
  | CH_EMAIL
  | CH_WHATSAPP
  | CH_WEB
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationContentTypeEnum
  = CONTENT_TEXT
  | CONTENT_IMAGE
  | CONTENT_VIDEO
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationDeliveryStatusType
  = DLV_PENDING
  | DLV_SENT
  | DLV_DELIVERED
  | DLV_READ
  | DLV_FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationDomainType
  = COMM_FLEET
  | COMM_RIDE_HAILING
  | COMM_GENERAL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationInfoResponse = CommunicationInfoResponse
  { id :: Kernel.Types.Id.Id Dashboard.Common.Communication,
    title :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    htmlBody :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    msgContentType :: CommunicationContentTypeEnum,
    mediaUrls :: Kernel.Prelude.Maybe [CommunicationMediaFile],
    channels :: [CommunicationChannelType],
    ctaButton :: Kernel.Prelude.Maybe CTAButtonReq,
    variables :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    domain :: CommunicationDomainType,
    senderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderRole :: CommunicationRoleType,
    status :: CommunicationStatusType,
    deliverySummary :: Kernel.Prelude.Maybe DeliverySummary,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationListItem = CommunicationListItem
  { id :: Kernel.Types.Id.Id Dashboard.Common.Communication,
    title :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    msgContentType :: CommunicationContentTypeEnum,
    channels :: [CommunicationChannelType],
    status :: CommunicationStatusType,
    domain :: CommunicationDomainType,
    senderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderRole :: CommunicationRoleType,
    deliverySummary :: Kernel.Prelude.Maybe DeliverySummary,
    isRead :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    channel :: Kernel.Prelude.Maybe CommunicationChannelType,
    createdAt :: Kernel.Prelude.UTCTime,
    sentAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationListResponse = CommunicationListResponse {communications :: [CommunicationListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationListType
  = LIST_SENT
  | LIST_RECEIVED
  | LIST_DRAFT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationMediaFile = CommunicationMediaFile {url :: Kernel.Prelude.Text, fileType :: Kernel.Prelude.Text, thumbnailUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationMessageStatus
  = SEND_NOW
  | SAVE_AS_DRAFT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommunicationRoleType
  = ROLE_DRIVER
  | ROLE_FLEET_OWNER
  | ROLE_OPERATOR
  | ROLE_ADMIN
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationStatusType
  = CS_DRAFT
  | CS_QUEUED
  | CS_SENDING
  | CS_SENT
  | CS_FAILED
  | CS_SCHEDULED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CommunicationTemplateResponse = CommunicationTemplateResponse {templateId :: Kernel.Prelude.Text, messageBody :: Kernel.Prelude.Text, templateName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCommunicationRequest = CreateCommunicationRequest
  { title :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    htmlBody :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    msgContentType :: CommunicationContentTypeEnum,
    mediaFileIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Dashboard.Common.File],
    channels :: [CommunicationChannelType],
    ctaButton :: Kernel.Prelude.Maybe CTAButtonReq,
    variables :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    domain :: CommunicationDomainType,
    recipientIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    selectAll :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    selectAllRoles :: Kernel.Prelude.Maybe [CommunicationRoleType],
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    messageStatus :: CommunicationMessageStatus,
    templateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    templateName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCommunicationRequest where
  hideSecrets = Kernel.Prelude.identity

data CreateCommunicationResponse = CreateCommunicationResponse {communicationId :: Kernel.Types.Id.Id Dashboard.Common.Communication, status :: CommunicationStatusType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeliveryStatusItem = DeliveryStatusItem
  { id :: Kernel.Prelude.Text,
    recipientId :: Kernel.Prelude.Text,
    recipientName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recipientRole :: CommunicationRoleType,
    channel :: CommunicationChannelType,
    status :: CommunicationDeliveryStatusType,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    readAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeliveryStatusResponse = DeliveryStatusResponse {deliveries :: [DeliveryStatusItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeliverySummary = DeliverySummary {total :: Kernel.Prelude.Int, sent :: Kernel.Prelude.Int, delivered :: Kernel.Prelude.Int, read :: Kernel.Prelude.Int, failed :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EditCommunicationRequest = EditCommunicationRequest
  { title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    body :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    htmlBody :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    msgContentType :: Kernel.Prelude.Maybe CommunicationContentTypeEnum,
    mediaFileIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Dashboard.Common.File],
    channels :: Kernel.Prelude.Maybe [CommunicationChannelType],
    ctaButton :: Kernel.Prelude.Maybe CTAButtonReq,
    variables :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets EditCommunicationRequest where
  hideSecrets = Kernel.Prelude.identity

data RecipientItem = RecipientItem
  { id :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    role :: CommunicationRoleType,
    phone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecipientsResponse = RecipientsResponse {recipients :: [RecipientItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SendCommunicationRequest = SendCommunicationRequest
  { recipientIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    selectAll :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    selectAllRoles :: Kernel.Prelude.Maybe [CommunicationRoleType],
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SendCommunicationRequest where
  hideSecrets = Kernel.Prelude.identity

type API = ("communication" :> (PostCommunicationCreate :<|> GetCommunicationList :<|> GetCommunicationInfo :<|> PostCommunicationSend :<|> PutCommunicationEdit :<|> DeleteCommunicationDelete :<|> GetCommunicationDeliveryStatus :<|> GetCommunicationRecipients :<|> GetCommunicationTemplate))

type PostCommunicationCreate =
  ( "create" :> MandatoryQueryParam "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> ReqBody '[JSON] CreateCommunicationRequest
      :> Post
           '[JSON]
           CreateCommunicationResponse
  )

type GetCommunicationList =
  ( "list" :> QueryParam "listType" CommunicationListType :> QueryParam "channel" CommunicationChannelType
      :> QueryParam
           "domain"
           CommunicationDomainType
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "personId"
           (Kernel.Types.Id.Id Dashboard.Common.Person)
      :> Get
           '[JSON]
           CommunicationListResponse
  )

type GetCommunicationInfo = (Capture "communicationId" (Kernel.Types.Id.Id Dashboard.Common.Communication) :> "info" :> Get '[JSON] CommunicationInfoResponse)

type PostCommunicationSend =
  ( Capture "communicationId" (Kernel.Types.Id.Id Dashboard.Common.Communication) :> "send" :> ReqBody '[JSON] SendCommunicationRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PutCommunicationEdit =
  ( Capture "communicationId" (Kernel.Types.Id.Id Dashboard.Common.Communication) :> "edit" :> ReqBody '[JSON] EditCommunicationRequest
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteCommunicationDelete = (Capture "communicationId" (Kernel.Types.Id.Id Dashboard.Common.Communication) :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetCommunicationDeliveryStatus =
  ( Capture "communicationId" (Kernel.Types.Id.Id Dashboard.Common.Communication) :> "deliveryStatus"
      :> QueryParam
           "channel"
           CommunicationChannelType
      :> QueryParam "status" CommunicationDeliveryStatusType
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           DeliveryStatusResponse
  )

type GetCommunicationRecipients =
  ( "recipients" :> QueryParam "role" CommunicationRoleType :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam "selectAll" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           RecipientsResponse
  )

type GetCommunicationTemplate =
  ( "template" :> MandatoryQueryParam "domain" CommunicationDomainType :> MandatoryQueryParam "channel" CommunicationChannelType
      :> Get
           '[JSON]
           CommunicationTemplateResponse
  )

data CommunicationAPIs = CommunicationAPIs
  { postCommunicationCreate :: Kernel.Types.Id.Id Dashboard.Common.Person -> CreateCommunicationRequest -> EulerHS.Types.EulerClient CreateCommunicationResponse,
    getCommunicationList :: Kernel.Prelude.Maybe CommunicationListType -> Kernel.Prelude.Maybe CommunicationChannelType -> Kernel.Prelude.Maybe CommunicationDomainType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient CommunicationListResponse,
    getCommunicationInfo :: Kernel.Types.Id.Id Dashboard.Common.Communication -> EulerHS.Types.EulerClient CommunicationInfoResponse,
    postCommunicationSend :: Kernel.Types.Id.Id Dashboard.Common.Communication -> SendCommunicationRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    putCommunicationEdit :: Kernel.Types.Id.Id Dashboard.Common.Communication -> EditCommunicationRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteCommunicationDelete :: Kernel.Types.Id.Id Dashboard.Common.Communication -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getCommunicationDeliveryStatus :: Kernel.Types.Id.Id Dashboard.Common.Communication -> Kernel.Prelude.Maybe CommunicationChannelType -> Kernel.Prelude.Maybe CommunicationDeliveryStatusType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient DeliveryStatusResponse,
    getCommunicationRecipients :: Kernel.Prelude.Maybe CommunicationRoleType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient RecipientsResponse,
    getCommunicationTemplate :: CommunicationDomainType -> CommunicationChannelType -> EulerHS.Types.EulerClient CommunicationTemplateResponse
  }

mkCommunicationAPIs :: (Client EulerHS.Types.EulerClient API -> CommunicationAPIs)
mkCommunicationAPIs communicationClient = (CommunicationAPIs {..})
  where
    postCommunicationCreate :<|> getCommunicationList :<|> getCommunicationInfo :<|> postCommunicationSend :<|> putCommunicationEdit :<|> deleteCommunicationDelete :<|> getCommunicationDeliveryStatus :<|> getCommunicationRecipients :<|> getCommunicationTemplate = communicationClient

data CommunicationUserActionType
  = POST_COMMUNICATION_CREATE
  | GET_COMMUNICATION_LIST
  | GET_COMMUNICATION_INFO
  | POST_COMMUNICATION_SEND
  | PUT_COMMUNICATION_EDIT
  | DELETE_COMMUNICATION_DELETE
  | GET_COMMUNICATION_DELIVERY_STATUS
  | GET_COMMUNICATION_RECIPIENTS
  | GET_COMMUNICATION_TEMPLATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''CommunicationChannelType)

$(mkHttpInstancesForEnum ''CommunicationContentTypeEnum)

$(mkHttpInstancesForEnum ''CommunicationDeliveryStatusType)

$(mkHttpInstancesForEnum ''CommunicationDomainType)

$(mkHttpInstancesForEnum ''CommunicationListType)

$(mkHttpInstancesForEnum ''CommunicationRoleType)

$(mkHttpInstancesForEnum ''CommunicationStatusType)

$(Data.Singletons.TH.genSingletons [''CommunicationUserActionType])
