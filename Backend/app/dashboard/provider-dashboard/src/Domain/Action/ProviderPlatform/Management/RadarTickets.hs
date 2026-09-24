module Domain.Action.ProviderPlatform.Management.RadarTickets
  ( getRadarTicketsList,
    getRadarTicketsSummary,
    getRadarTicketsReporters,
    getRadarTicketsTicket,
    getRadarTicketsTicketConversation,
    getRadarTicketsTicketAttachment,
    postRadarTicketsTicketReply,
    postRadarTicketsUpload,
    postRadarTicketsCreate,
    postRadarTicketsTicketUpdateStatus,
    postRadarTicketsTicketCsat,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.RadarTickets
import qualified Data.Text as T
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
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
import Tools.Auth.Merchant

-- | Desk writes are attributed to the operator who clicked, not to the
-- frontend's say-so: sender identity comes from the verified session person,
-- and whatever the client sent in those fields is discarded.
sessionIdentity :: ApiTokenInfo UserActionType -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text)
sessionIdentity apiTokenInfo = do
  mbEmail <- mapM decrypt apiTokenInfo.person.email
  let name = T.strip (apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName)
  pure (mbEmail, if T.null name then Kernel.Prelude.Nothing else Kernel.Prelude.Just name)

getRadarTicketsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketListRes)
getRadarTicketsList merchantShortId opCity apiTokenInfo pageSize cursor priority stageName reporterEmail = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsList) pageSize cursor priority stageName reporterEmail

getRadarTicketsSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarSummaryRes)
getRadarTicketsSummary merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsSummary)

getRadarTicketsReporters :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarReportersRes)
getRadarTicketsReporters merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsReporters)

getRadarTicketsTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarTicketDetailRes)
getRadarTicketsTicket merchantShortId opCity apiTokenInfo ticketId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicket) ticketId

getRadarTicketsTicketConversation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarConversationRes)
getRadarTicketsTicketConversation merchantShortId opCity apiTokenInfo ticketId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicketConversation) ticketId

getRadarTicketsTicketAttachment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarAttachmentUrlRes)
getRadarTicketsTicketAttachment merchantShortId opCity apiTokenInfo ticketId attachmentId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.getRadarTicketsTicketAttachment) ticketId attachmentId

postRadarTicketsTicketReply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketReply merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbEmail, mbName) <- sessionIdentity apiTokenInfo
  let filledReq = API.Types.ProviderPlatform.Management.RadarTickets.RadarReplyReq {body = req.body, senderEmail = mbEmail, senderName = mbName}
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just filledReq)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketReply) ticketId filledReq)

postRadarTicketsUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadReq -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarUploadRes)
postRadarTicketsUpload merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- Audit the action but never the payload: storing the request would put the
  -- full base64 file body in the transaction table.
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsUpload) req)

postRadarTicketsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq -> Environment.Flow API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketRes)
postRadarTicketsCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbEmail, mbName) <- sessionIdentity apiTokenInfo
  let filledReq =
        API.Types.ProviderPlatform.Management.RadarTickets.RadarCreateTicketReq
          { category = req.category,
            subCategory = req.subCategory,
            title = req.title,
            citiesAffected = req.citiesAffected,
            whereSeen = req.whereSeen,
            stillHappening = req.stillHappening,
            urgency = req.urgency,
            body = req.body,
            context = req.context,
            attachmentFileIds = req.attachmentFileIds,
            senderEmail = mbEmail,
            senderName = mbName
          }
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just filledReq)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsCreate) filledReq)

postRadarTicketsTicketUpdateStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarUpdateStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketUpdateStatus merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketUpdateStatus) ticketId req)

postRadarTicketsTicketCsat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RadarTickets.RadarCsatReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRadarTicketsTicketCsat merchantShortId opCity apiTokenInfo ticketId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.radarTicketsDSL.postRadarTicketsTicketCsat) ticketId req)
