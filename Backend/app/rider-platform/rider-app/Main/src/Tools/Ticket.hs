{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Ticket
  ( createTicket,
    updateSosTicket,
    createSosTicket,
    updateTicket,
    updateTicketOnService,
    updateTicketStatus,
    addAndUpdateKaptureCustomer,
    kaptureEncryption,
    kapturePullTicket,
    kaptureGetTicket,
    getTicketStatus,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import EulerHS.Prelude
import qualified IssueManagement.Common as ICommon
import qualified Kernel.External.Ticket.Interface as TI
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.Types as TicketTypes
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))

-- | Regular-issue create-ticket entry point. Calls the primary provider
-- (blocking) and, when the merchant is configured with
-- @additionalIssueTicketServices@, also fans out to every secondary provider
-- best-effort. Returns the primary provider's response paired with the list
-- of @(service, ticketId)@ pairs produced by successful secondary calls so
-- the caller (shared 'createIssueReport') can persist the mapping for future
-- update-ticket calls. Secondary failures are logged and never propagate.
createTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.CreateTicketReq -> m (Ticket.CreateTicketResp, [ICommon.AdditionalTicketId])
createTicket merchantId mocId req = do
  merchantConfig <- fetchUsageConfig mocId
  let primary = merchantConfig.issueTicketService
      secondaries = filter (/= primary) . fromMaybe [] $ merchantConfig.additionalIssueTicketServices
  -- Primary failure must not swallow the secondary fan-out. If the primary
  -- provider is broken (e.g. Kapture returning 404) but the merchant has
  -- XyneSpaces configured as secondary, we still want the ticket to land on
  -- Xyne and be persisted as the effective 'issueReport.ticketId' so the
  -- chat-forwarding path can then update the SAME Xyne thread instead of
  -- opening a new one on every message.
  primaryResult <- withTryCatch "createTicket:primary" (callCreateTicket merchantId mocId primary req)
  secondaryResults <- fanOutCreates merchantId mocId secondaries req
  case primaryResult of
    Right primaryResp -> do
      let asAdditional = [ICommon.AdditionalTicketId {service = svc, ticketId = r.ticketId} | (svc, r) <- secondaryResults]
      pure (primaryResp, asAdditional)
    Left primaryErr -> case secondaryResults of
      ((promotedSvc, promotedResp) : rest) -> do
        logError $ "createTicket:primary " <> show primary <> " failed, promoting secondary " <> show promotedSvc <> " as effective ticket. err=" <> show primaryErr
        let asAdditional = [ICommon.AdditionalTicketId {service = svc, ticketId = r.ticketId} | (svc, r) <- rest]
        pure (promotedResp, asAdditional)
      [] -> do
        logError $ "createTicket:primary failed and no secondaries configured/succeeded. err=" <> show primaryErr
        -- Re-throw as an InternalError so the shared 'withTryCatch' at the
        -- call site sees it as a normal failure; SomeException isn't a
        -- Kernel error type accepted by 'throwError'.
        throwError $ InternalError $ "createTicket failed: " <> show primaryErr

-- | SOS create-ticket entry point. Deliberately does NOT fan out — the SOS
-- flow was scoped to the single-provider behavior when this fan-out was
-- introduced. If the fan-out should later apply to SOS as well, extend
-- 'MerchantServiceUsageConfig' with an @additionalSosTicketServices@ list.
createSosTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.CreateTicketReq -> m Ticket.CreateTicketResp
createSosTicket = resolveAndCallTicketService (\cfg -> fromMaybe cfg.issueTicketService cfg.sosTicketService) TI.createTicket

-- | Regular-issue update-ticket entry point. Updates the primary provider's
-- ticket (using 'req.ticketId') and, when @additionalTicketIds@ is non-empty,
-- also updates every secondary provider's mirrored ticket best-effort. The
-- caller (shared 'updateTicketStatus') is expected to pass through the
-- 'additionalTicketIds' persisted at create time on 'IssueReport'; pass @[]@
-- to hit the primary only (e.g. the chat-forwarding path targets Xyne
-- directly via 'updateTicketOnService' instead).
updateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [ICommon.AdditionalTicketId] -> Ticket.UpdateTicketReq -> m Ticket.UpdateTicketResp
updateTicket merchantId mocId additionalTicketIds req = do
  merchantConfig <- fetchUsageConfig mocId
  let primary = merchantConfig.issueTicketService
  primaryResp <- callUpdateTicket merchantId mocId primary req
  fanOutUpdates merchantId mocId primary additionalTicketIds req
  pure primaryResp

updateSosTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.UpdateTicketReq -> m Ticket.UpdateTicketResp
updateSosTicket = resolveAndCallTicketService (\cfg -> fromMaybe cfg.issueTicketService cfg.sosTicketService) TI.updateTicket

-- | Update a ticket on a specific configured provider, bypassing the
-- primary/secondary dispatch. Used by the shared chat-forwarding path to
-- target XyneSpaces even when it runs as a secondary alongside a primary
-- Zendesk (which does not want duplicate comments).
updateTicketOnService :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> TicketTypes.IssueTicketService -> Ticket.UpdateTicketReq -> m Ticket.UpdateTicketResp
updateTicketOnService = callUpdateTicket

-- | Status-only update. Targets XyneSpaces directly since it is the only
-- provider with a dedicated status endpoint; a merchant whose XyneSpaces
-- runs as a secondary (e.g. primary=Zendesk) still gets the status sync.
updateTicketStatus :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.UpdateTicketStatusReq -> m ()
updateTicketStatus merchantId mocId = callServiceSpecific TI.updateTicketStatus merchantId mocId TicketTypes.XyneSpaces

addAndUpdateKaptureCustomer :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.KaptureCustomerReq -> m Ticket.KaptureCustomerResp
addAndUpdateKaptureCustomer = resolveAndCallTicketService (.issueTicketService) TI.addAndUpdateKaptureCustomer

kaptureEncryption :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.KaptureEncryptionReq -> m Ticket.KaptureEncryptionResp
kaptureEncryption = resolveAndCallTicketService (.issueTicketService) TI.kaptureEncryption

kapturePullTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.KapturePullTicketReq -> m Ticket.KapturePullTicketResp
kapturePullTicket = resolveAndCallTicketService (.issueTicketService) TI.kapturePullTicket

kaptureGetTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.GetTicketReq -> m [Ticket.GetTicketResp]
kaptureGetTicket = resolveAndCallTicketService (.issueTicketService) TI.kaptureGetTicket

getTicketStatus :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.SearchTicketByIdReq -> m [Ticket.GetTicketStatusResp]
getTicketStatus = resolveAndCallTicketService (.issueTicketService) TI.getTicketStatus

-- | Iterates over secondary services and calls 'TI.createTicket' on each,
-- returning full '(service, response)' pairs so 'createTicket' can promote
-- the first success as the effective ticket when the primary fails.
-- Individual failures are swallowed with a log line so a flaky provider
-- cannot break the fan-out.
fanOutCreates ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  [TicketTypes.IssueTicketService] ->
  Ticket.CreateTicketReq ->
  m [(TicketTypes.IssueTicketService, Ticket.CreateTicketResp)]
fanOutCreates merchantId mocId services req = fmap catMaybes . forM services $ \svc -> do
  result <- withTryCatch "createTicket:secondary" (callCreateTicket merchantId mocId svc req)
  case result of
    Right resp -> do
      logInfo $ "createTicket:secondary success service=" <> show svc <> " ticketId=" <> resp.ticketId
      pure $ Just (svc, resp)
    Left err -> do
      logError $ "createTicket:secondary failed service=" <> show svc <> " mocId=" <> mocId.getId <> " err=" <> show err
      pure Nothing

-- | Iterates over the ticketIds captured at create time and updates each
-- secondary provider's mirrored ticket. Skips any entry whose service equals
-- the primary (already updated by the caller). Failures are swallowed.
fanOutUpdates ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TicketTypes.IssueTicketService ->
  [ICommon.AdditionalTicketId] ->
  Ticket.UpdateTicketReq ->
  m ()
fanOutUpdates merchantId mocId primary additionalTicketIds req =
  forM_ additionalTicketIds $ \entry ->
    when (entry.service /= primary) $ do
      -- Retarget the update request at this provider's own ticketId; the
      -- comment, status, media, etc. stay the same across providers.
      let secondaryReq = (req :: Ticket.UpdateTicketReq) {Ticket.ticketId = entry.ticketId}
      result <- withTryCatch "updateTicket:secondary" (callUpdateTicket merchantId mocId entry.service secondaryReq)
      case result of
        Right _ -> logInfo $ "updateTicket:secondary success service=" <> show entry.service <> " ticketId=" <> entry.ticketId
        Left err -> logError $ "updateTicket:secondary failed service=" <> show entry.service <> " ticketId=" <> entry.ticketId <> " err=" <> show err

callCreateTicket ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TicketTypes.IssueTicketService ->
  Ticket.CreateTicketReq ->
  m Ticket.CreateTicketResp
callCreateTicket = callServiceSpecific TI.createTicket

callUpdateTicket ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TicketTypes.IssueTicketService ->
  Ticket.UpdateTicketReq ->
  m Ticket.UpdateTicketResp
callUpdateTicket = callServiceSpecific TI.updateTicket

-- | Looks up a specific provider's config in 'MerchantServiceConfig' (by
-- 'IssueTicketService' enum) and invokes the provider implementation. Used
-- for both primary and secondary calls in the fan-out — the choice of which
-- service to hit is made by the caller, not by 'MerchantServiceUsageConfig'.
callServiceSpecific ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (Ticket.IssueTicketServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TicketTypes.IssueTicketService ->
  req ->
  m resp
callServiceSpecific func merchantId merchantOperatingCityId service req = do
  merchantIssueTicketServiceConfig <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.IssueTicketService service)})
      (Just (maybeToList <$> CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.IssueTicketService service)))
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  case merchantIssueTicketServiceConfig.serviceConfig of
    DMSC.IssueTicketServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

fetchUsageConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m DMSUC.MerchantServiceUsageConfig
fetchUsageConfig merchantOperatingCityId =
  getConfig
    (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId})
    (Just (CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId))
    >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)

resolveAndCallTicketService ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (DMSUC.MerchantServiceUsageConfig -> TicketTypes.IssueTicketService) ->
  (Ticket.IssueTicketServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
resolveAndCallTicketService selectService func merchantId merchantOperatingCityId req = do
  merchantConfig <- fetchUsageConfig merchantOperatingCityId
  let service = selectService merchantConfig
  logDebug $ "resolveAndCallTicketService: service=" <> show service <> " mocId=" <> merchantOperatingCityId.getId
  callServiceSpecific func merchantId merchantOperatingCityId service req
