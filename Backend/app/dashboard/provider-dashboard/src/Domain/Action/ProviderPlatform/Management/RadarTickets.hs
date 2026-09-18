{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.RadarTickets
  ( getRadarTicketsList,
    getRadarTicketsSummary,
    getRadarTicketsReporters,
    getRadarTicketsTicket,
    getRadarTicketsTicketAttachment,
    getRadarTicketsTicketConversation,
    postRadarTicketsTicketReply,
    postRadarTicketsCreate,
    postRadarTicketsTicketUpdateStatus,
    postRadarTicketsTicketCsat,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.RadarTickets
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getRadarTicketsList :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketListRes
getRadarTicketsList merchantShortId opCity apiTokenInfo pageSize cursor priority stageName reporterEmail = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsList) pageSize cursor priority stageName reporterEmail

getRadarTicketsSummary :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarSummaryRes
getRadarTicketsSummary merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsSummary)

getRadarTicketsReporters :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarReportersRes
getRadarTicketsReporters merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsReporters)

getRadarTicketsTicket :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketDetailRes
getRadarTicketsTicket merchantShortId opCity apiTokenInfo ticketId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicket) ticketId

getRadarTicketsTicketAttachment :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarAttachmentUrlRes
getRadarTicketsTicketAttachment merchantShortId opCity apiTokenInfo ticketId attachmentId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicketAttachment) ticketId attachmentId

getRadarTicketsTicketConversation :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarConversationRes
getRadarTicketsTicketConversation merchantShortId opCity apiTokenInfo ticketId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicketConversation) ticketId

postRadarTicketsTicketReply :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postRadarTicketsTicketReply merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (senderEmail, senderName) <- senderIdentity apiTokenInfo
  let enrichedReq =
        API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq
          { body = req.body,
            senderEmail = senderEmail,
            senderName = senderName
          }
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketReply) ticketId enrichedReq)

postRadarTicketsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketRes
postRadarTicketsCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (senderEmail, senderName) <- senderIdentity apiTokenInfo
  let enrichedReq =
        API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq
          { subject = req.subject,
            body = req.body,
            priority = req.priority,
            senderEmail = senderEmail,
            senderName = senderName
          }
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsCreate) enrichedReq)

postRadarTicketsTicketUpdateStatus :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUpdateStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postRadarTicketsTicketUpdateStatus merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketUpdateStatus) ticketId req)

postRadarTicketsTicketCsat :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCsatReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postRadarTicketsTicketCsat merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketCsat) ticketId req)

-- | Replier identity comes from the verified session, never from the client
-- payload: the driver-app side has no dashboard-person context, so the
-- decrypted email and display name must travel in the request.
senderIdentity :: ApiTokenInfo -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text)
senderIdentity apiTokenInfo = do
  senderEmail <- mapM decrypt apiTokenInfo.person.email
  let senderName = T.strip (apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName)
  pure (senderEmail, if T.null senderName then Kernel.Prelude.Nothing else Kernel.Prelude.Just senderName)
