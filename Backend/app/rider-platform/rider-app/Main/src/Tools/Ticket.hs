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
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface as TI
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC

createTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Ticket.CreateTicketReq -> m Ticket.CreateTicketResp
createTicket = runWithServiceConfig TI.createTicket

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (Ticket.IssueTicketServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  req ->
  m resp
runWithServiceConfig func merchantId req = do
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  merchantIssueTicketServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.IssueTicketService merchantConfig.issueTicketService)
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  case merchantIssueTicketServiceConfig.serviceConfig of
    DMSC.IssueTicketServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
