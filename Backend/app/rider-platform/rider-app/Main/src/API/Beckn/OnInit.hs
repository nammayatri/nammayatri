{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnInit (API, handler) where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnInit as TaxiACL
import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified BecknV2.OnDemand.Utils.Common as Common
import BecknV2.Utils
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Types.CancellationReason
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API = OnInit.OnInitAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReqV2 ->
  FlowHandler AckResponse
onInit _ reqV2 = withFlowHandlerBecknAPI $ do
  transactionId <- Common.getTransactionId reqV2.onInitReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    mbDOnInitReq <- TaxiACL.buildOnInitReqV2 reqV2
    whenJust mbDOnInitReq $ \onInitReq ->
      Redis.whenWithLockRedis (onInitLockKey onInitReq.bppBookingId.getId) 60 $
        fork "oninit request processing" $ do
          (onInitRes, booking) <- DOnInit.onInit onInitReq
          becknConfig <- QBC.findByMerchantIdDomainAndVehicle onInitRes.merchant.id "MOBILITY" (UCommon.mapVariantToVehicle onInitRes.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
          fork "on init received pushing ondc logs" do
            let transactionLog = TransactionLogReq "on_init" $ ReqLog (toJSON reqV2.onInitReqContext) (maskSensitiveData $ toJSON reqV2.onInitReqMessage)
            void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
          handle (errHandler booking) . void . withShortRetry $ do
            confirmBecknReq <- ACL.buildConfirmReqV2 onInitRes
            fork "sending confirm, pushing ondc logs" do
              let transactionLog = TransactionLogReq "confirm" $ ReqLog (toJSON confirmBecknReq.confirmReqContext) (maskSensitiveData $ toJSON confirmBecknReq.confirmReqMessage)
              void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
            CallBPP.confirmV2 onInitRes.bppUrl confirmBecknReq
    pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes
      | otherwise = throwM exc

    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BppBookingId-" <> id
