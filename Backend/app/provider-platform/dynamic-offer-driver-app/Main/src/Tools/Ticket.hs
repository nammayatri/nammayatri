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
    updateTicket,
    addAndUpdateKaptureCustomer,
    kaptureEncryption,
    kapturePullTicket,
    kaptureGetTicket,
  )
where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantServiceConfig as DMSC
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface as Ticket
import Kernel.External.Ticket.Interface.Types as TIT
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC

createTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
createTicket = runWithServiceConfig Ticket.createTicket

updateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp
updateTicket = runWithServiceConfig Ticket.updateTicket

addAndUpdateKaptureCustomer :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.KaptureCustomerReq -> m TIT.KaptureCustomerResp
addAndUpdateKaptureCustomer = runWithServiceConfig Ticket.addAndUpdateKaptureCustomer

kaptureEncryption :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.KaptureEncryptionReq -> m TIT.KaptureEncryptionResp
kaptureEncryption = runWithServiceConfig Ticket.kaptureEncryption

kapturePullTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.KapturePullTicketReq -> m TIT.KapturePullTicketResp
kapturePullTicket = runWithServiceConfig Ticket.kapturePullTicket

kaptureGetTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> TIT.GetTicketReq -> m [TIT.GetTicketResp]
kaptureGetTicket = runWithServiceConfig Ticket.kaptureGetTicket

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  (TIT.IssueTicketServiceConfig -> req -> m resp) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchantId merchantOpCityId req = do
  merchantConfig <- QMSUC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantIssueTicketServiceConfig <-
    QMSC.findByServiceAndCity (DMSC.IssueTicketService merchantConfig.issueTicketService) merchantOpCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  case merchantIssueTicketServiceConfig.serviceConfig of
    DMSC.IssueTicketServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
