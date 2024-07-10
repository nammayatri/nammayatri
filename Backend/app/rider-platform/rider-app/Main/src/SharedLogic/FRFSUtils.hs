{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSUtils where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Station as Station
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import Tools.Error

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Id DPO.PartnerOrganization -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} pOrgId = do
  pOrgStation <- B.runInReplica $ CQPOS.findByStationIdAndPOrgId id pOrgId >>= fromMaybeM (PartnerOrgStationNotFoundForStationId pOrgId.getId id.getId)
  pure $ APITypes.FRFSStationAPI {name = pOrgStation.name, stationType = Nothing, color = Nothing, sequenceNum = Nothing, ..}

mkTBPStatusAPI :: DTBP.FRFSTicketBookingPaymentStatus -> APITypes.FRFSBookingPaymentStatusAPI
mkTBPStatusAPI = \case
  DTBP.PENDING -> APITypes.PENDING
  DTBP.SUCCESS -> APITypes.SUCCESS
  DTBP.FAILED -> APITypes.FAILURE
  DTBP.REFUND_PENDING -> APITypes.REFUND_PENDING
  DTBP.REFUNDED -> APITypes.REFUNDED

mkFRFSConfigAPI :: Config.FRFSConfig -> APITypes.FRFSConfigAPIRes
mkFRFSConfigAPI Config.FRFSConfig {..} = do
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, ..}
