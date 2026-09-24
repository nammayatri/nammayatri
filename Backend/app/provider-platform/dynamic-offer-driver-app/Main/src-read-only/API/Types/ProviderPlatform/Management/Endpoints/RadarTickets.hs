{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.RadarTickets where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data RadarAttachmentInfo = RadarAttachmentInfo {id :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, mimeType :: Kernel.Prelude.Maybe Kernel.Prelude.Text, size :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarAttachmentUrlRes = RadarAttachmentUrlRes {url :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarCategoryCount = RadarCategoryCount {category :: Kernel.Prelude.Text, count :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarConversationRes = RadarConversationRes {messages :: [RadarMessage], hasMore :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarCreateTicketReq = RadarCreateTicketReq
  { category :: Kernel.Prelude.Text,
    subCategory :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    citiesAffected :: [Kernel.Prelude.Text],
    whereSeen :: Kernel.Prelude.Text,
    stillHappening :: Kernel.Prelude.Text,
    urgency :: Kernel.Prelude.Text,
    body :: Kernel.Prelude.Text,
    context :: [RadarKV],
    attachmentFileIds :: [Kernel.Prelude.Text],
    senderEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RadarCreateTicketReq where
  hideSecrets = Kernel.Prelude.identity

data RadarCreateTicketRes = RadarCreateTicketRes {ticketId :: Kernel.Prelude.Text, xyneId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarCsatReq = RadarCsatReq {rating :: Kernel.Prelude.Text, score :: Kernel.Prelude.Int, comment :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RadarCsatReq where
  hideSecrets = Kernel.Prelude.identity

data RadarKV = RadarKV {key :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarMessage = RadarMessage
  { id :: Kernel.Prelude.Text,
    from :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bodyHtml :: Kernel.Prelude.Text,
    attachments :: [RadarAttachmentInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarReplyReq = RadarReplyReq {body :: Kernel.Prelude.Text, senderEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text, senderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RadarReplyReq where
  hideSecrets = Kernel.Prelude.identity

data RadarReporter = RadarReporter {email :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarReportersRes = RadarReportersRes {reporters :: [RadarReporter]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarSummaryRes = RadarSummaryRes {tiles :: RadarSummaryTiles, recent :: [RadarTicketItem], byCategory :: [RadarCategoryCount]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarSummaryTiles = RadarSummaryTiles {total :: Kernel.Prelude.Int, backlog :: Kernel.Prelude.Int, inProgress :: Kernel.Prelude.Int, completed :: Kernel.Prelude.Int, notRequired :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarTicketDetailRes = RadarTicketDetailRes
  { id :: Kernel.Prelude.Text,
    xyneId :: Kernel.Prelude.Text,
    subject :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Text,
    statusV2 :: Kernel.Prelude.Text,
    stageName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.Text,
    lastEmailAt :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reporterEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    emailCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    history :: [RadarTicketEvent],
    messages :: [RadarMessage]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarTicketEvent = RadarTicketEvent
  { id :: Kernel.Prelude.Text,
    eventType :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Text,
    at :: Kernel.Prelude.Text,
    actor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    from :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    to :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarTicketItem = RadarTicketItem
  { id :: Kernel.Prelude.Text,
    xyneId :: Kernel.Prelude.Text,
    subject :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Text,
    statusV2 :: Kernel.Prelude.Text,
    stageName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.Text,
    lastEmailAt :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarTicketListRes = RadarTicketListRes {items :: [RadarTicketItem], hasMore :: Kernel.Prelude.Bool, nextCursor :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadarUpdateStatusReq = RadarUpdateStatusReq {stageName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RadarUpdateStatusReq where
  hideSecrets = Kernel.Prelude.identity

data RadarUploadReq = RadarUploadReq {fileBase64 :: Kernel.Prelude.Text, mimeType :: Kernel.Prelude.Text, fileName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RadarUploadReq where
  hideSecrets = Kernel.Prelude.identity

data RadarUploadRes = RadarUploadRes {fileId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("radarTickets" :> (GetRadarTicketsList :<|> GetRadarTicketsSummary :<|> GetRadarTicketsReporters :<|> GetRadarTicketsTicket :<|> GetRadarTicketsTicketConversation :<|> GetRadarTicketsTicketAttachment :<|> PostRadarTicketsTicketReply :<|> PostRadarTicketsUpload :<|> PostRadarTicketsCreate :<|> PostRadarTicketsTicketUpdateStatus :<|> PostRadarTicketsTicketCsat))

type GetRadarTicketsList =
  ( "list" :> QueryParam "pageSize" Kernel.Prelude.Int :> QueryParam "cursor" Kernel.Prelude.Text :> QueryParam "priority" Kernel.Prelude.Text
      :> QueryParam
           "stageName"
           Kernel.Prelude.Text
      :> QueryParam "reporterEmail" Kernel.Prelude.Text
      :> Get ('[JSON]) RadarTicketListRes
  )

type GetRadarTicketsSummary = ("summary" :> Get ('[JSON]) RadarSummaryRes)

type GetRadarTicketsReporters = ("reporters" :> Get ('[JSON]) RadarReportersRes)

type GetRadarTicketsTicket = ("ticket" :> Capture "ticketId" Kernel.Prelude.Text :> Get ('[JSON]) RadarTicketDetailRes)

type GetRadarTicketsTicketConversation = ("ticket" :> Capture "ticketId" Kernel.Prelude.Text :> "conversation" :> Get ('[JSON]) RadarConversationRes)

type GetRadarTicketsTicketAttachment = ("ticket" :> Capture "ticketId" Kernel.Prelude.Text :> "attachment" :> Capture "attachmentId" Kernel.Prelude.Text :> Get ('[JSON]) RadarAttachmentUrlRes)

type PostRadarTicketsTicketReply = ("ticket" :> Capture "ticketId" Kernel.Prelude.Text :> "reply" :> ReqBody ('[JSON]) RadarReplyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostRadarTicketsUpload = ("upload" :> ReqBody ('[JSON]) RadarUploadReq :> Post ('[JSON]) RadarUploadRes)

type PostRadarTicketsCreate = ("create" :> ReqBody ('[JSON]) RadarCreateTicketReq :> Post ('[JSON]) RadarCreateTicketRes)

type PostRadarTicketsTicketUpdateStatus =
  ( "ticket" :> Capture "ticketId" Kernel.Prelude.Text :> "updateStatus" :> ReqBody ('[JSON]) RadarUpdateStatusReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostRadarTicketsTicketCsat = ("ticket" :> Capture "ticketId" Kernel.Prelude.Text :> "csat" :> ReqBody ('[JSON]) RadarCsatReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data RadarTicketsAPIs = RadarTicketsAPIs
  { getRadarTicketsList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient RadarTicketListRes),
    getRadarTicketsSummary :: (EulerHS.Types.EulerClient RadarSummaryRes),
    getRadarTicketsReporters :: (EulerHS.Types.EulerClient RadarReportersRes),
    getRadarTicketsTicket :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient RadarTicketDetailRes),
    getRadarTicketsTicketConversation :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient RadarConversationRes),
    getRadarTicketsTicketAttachment :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient RadarAttachmentUrlRes),
    postRadarTicketsTicketReply :: (Kernel.Prelude.Text -> RadarReplyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRadarTicketsUpload :: (RadarUploadReq -> EulerHS.Types.EulerClient RadarUploadRes),
    postRadarTicketsCreate :: (RadarCreateTicketReq -> EulerHS.Types.EulerClient RadarCreateTicketRes),
    postRadarTicketsTicketUpdateStatus :: (Kernel.Prelude.Text -> RadarUpdateStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRadarTicketsTicketCsat :: (Kernel.Prelude.Text -> RadarCsatReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkRadarTicketsAPIs :: (Client EulerHS.Types.EulerClient API -> RadarTicketsAPIs)
mkRadarTicketsAPIs radarTicketsClient = (RadarTicketsAPIs {..})
  where
    getRadarTicketsList :<|> getRadarTicketsSummary :<|> getRadarTicketsReporters :<|> getRadarTicketsTicket :<|> getRadarTicketsTicketConversation :<|> getRadarTicketsTicketAttachment :<|> postRadarTicketsTicketReply :<|> postRadarTicketsUpload :<|> postRadarTicketsCreate :<|> postRadarTicketsTicketUpdateStatus :<|> postRadarTicketsTicketCsat = radarTicketsClient

data RadarTicketsUserActionType
  = GET_RADAR_TICKETS_LIST
  | GET_RADAR_TICKETS_SUMMARY
  | GET_RADAR_TICKETS_REPORTERS
  | GET_RADAR_TICKETS_TICKET
  | GET_RADAR_TICKETS_TICKET_CONVERSATION
  | GET_RADAR_TICKETS_TICKET_ATTACHMENT
  | POST_RADAR_TICKETS_TICKET_REPLY
  | POST_RADAR_TICKETS_UPLOAD
  | POST_RADAR_TICKETS_CREATE
  | POST_RADAR_TICKETS_TICKET_UPDATE_STATUS
  | POST_RADAR_TICKETS_TICKET_CSAT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RadarTicketsUserActionType)])
