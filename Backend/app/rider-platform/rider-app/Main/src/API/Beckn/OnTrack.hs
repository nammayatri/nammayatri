{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnTrack (API, handler) where

import qualified Beckn.ACL.OnTrack as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QRB
import Tools.Error
import Tools.TransactionLogs
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API = OnTrack.OnTrackAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onTrack

onTrack ::
  SignatureAuthResult ->
  OnTrack.OnTrackReqV2 ->
  FlowHandler AckResponse
onTrack _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onTrackReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    mbDOnTrackReq <- ACL.buildOnTrackReqV2 reqV2
    whenJust mbDOnTrackReq \onTrackReq -> do
      validatedReq <- DOnTrack.validateRequest onTrackReq
      fork "on track processing" $
        DOnTrack.onTrack validatedReq
      fork "on track received pushing ondc logs" do
        let kafkaLog = TransactionLog "on_track" $ Req reqV2.onTrackReqContext (toJSON reqV2.onTrackReqMessage)
        pushBecknLogToKafka kafkaLog
        booking <- QRB.findById validatedReq.ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BookingId:-" <> validatedReq.ride.bookingId.getId)
        let transactionLog = TransactionLogReq "on_track" $ ReqLog (toJSON reqV2.onTrackReqContext) (maskSensitiveData $ toJSON reqV2.onTrackReqMessage)
        becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.merchantId "MOBILITY" (Utils.mapVariantToVehicle validatedReq.ride.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
        void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
  pure Ack
