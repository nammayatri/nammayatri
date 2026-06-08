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
import qualified Kernel.External.Ticket.Interface as TI
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig, getOneConfig)

createTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.CreateTicketReq -> m Ticket.CreateTicketResp
createTicket = resolveAndCallTicketService (.issueTicketService) TI.createTicket

createSosTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.CreateTicketReq -> m Ticket.CreateTicketResp
createSosTicket = resolveAndCallTicketService (\cfg -> fromMaybe cfg.issueTicketService cfg.sosTicketService) TI.createTicket

updateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.UpdateTicketReq -> m Ticket.UpdateTicketResp
updateTicket = resolveAndCallTicketService (.issueTicketService) TI.updateTicket

updateSosTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Ticket.UpdateTicketReq -> m Ticket.UpdateTicketResp
updateSosTicket = resolveAndCallTicketService (\cfg -> fromMaybe cfg.issueTicketService cfg.sosTicketService) TI.updateTicket

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

resolveAndCallTicketService ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (DMSUC.MerchantServiceUsageConfig -> Kernel.External.Ticket.Types.IssueTicketService) ->
  (Ticket.IssueTicketServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
resolveAndCallTicketService selectService func merchantId merchantOperatingCityId req = do
  merchantConfig <-
    getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId})
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  let service = selectService merchantConfig
  logDebug $ "resolveAndCallTicketService: service=" <> show service <> " mocId=" <> merchantOperatingCityId.getId
  merchantIssueTicketServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.IssueTicketService service)})
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  logDebug $ "resolveAndCallTicketService: serviceConfig resolved, calling provider"
  case merchantIssueTicketServiceConfig.serviceConfig of
    DMSC.IssueTicketServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
