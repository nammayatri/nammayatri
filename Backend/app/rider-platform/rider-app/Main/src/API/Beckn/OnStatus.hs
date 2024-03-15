{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnStatus (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QRB
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API = OnStatus.OnStatusAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  OnStatus.OnStatusReqV2 ->
  FlowHandler AckResponse
onStatus _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onStatusReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    messageId <- Utils.getMessageIdText reqV2.onStatusReqContext
    mbDOnStatusReq <- ACL.buildOnStatusReqV2 reqV2
    whenJust mbDOnStatusReq $ \onStatusReq ->
      Redis.whenWithLockRedis (onStatusLockKey messageId) 60 $ do
        validatedOnStatusReq <- DOnStatus.validateRequest onStatusReq
        fork "on update processing" $ do
          Redis.whenWithLockRedis (onStatusProcessngLockKey messageId) 60 $
            DOnStatus.onStatus validatedOnStatusReq
        fork "on status received pushing ondc logs" do
          (variant, merchantId) <- do
            booking <- QRB.findByBPPBookingId onStatusReq.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> onStatusReq.bppBookingId.getId)
            return (booking.vehicleVariant, booking.merchantId)
          let transactionLog = TransactionLogReq "on_status" $ ReqLog (toJSON reqV2.onStatusReqContext) (maskSensitiveData $ toJSON reqV2.onStatusReqMessage)
          becknConfig <- QBC.findByMerchantIdDomainAndVehicle merchantId "MOBILITY" (Utils.mapVariantToVehicle variant) >>= fromMaybeM (InternalError "Beckn Config not found")
          void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
  pure Ack

onStatusLockKey :: Text -> Text
onStatusLockKey id = "Customer:OnStatus:MessageId-" <> id

onStatusProcessngLockKey :: Text -> Text
onStatusProcessngLockKey id = "Customer:OnStatus:Processing:MessageId-" <> id
