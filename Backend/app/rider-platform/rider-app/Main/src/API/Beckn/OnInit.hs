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
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified BecknV2.OnDemand.Utils.Common as Common
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..), CancellationStage (..))
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Booking as QRB
import qualified Tools.Metrics as Metrics
import TransactionLogs.PushLogs

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
    if isJust mbDOnInitReq
      then do
        let onInitReq = fromJust mbDOnInitReq -- safe to use here, because of above check
        Redis.whenWithLockRedis (onInitLockKey onInitReq.bookingId.getId) 60 $
          fork "on_init request processing" $ do
            (onInitRes, booking) <- DOnInit.onInit onInitReq
            fork "on init received pushing ondc logs" do
              void $ pushLogs "on_init" (toJSON reqV2) onInitRes.merchant.id.getId "MOBILITY"
            handle (errHandler booking) . void . withShortRetry $ do
              confirmBecknReq <- ACL.buildConfirmReqV2 onInitRes
              Metrics.startMetricsBap Metrics.CONFIRM onInitRes.merchant.name transactionId booking.merchantOperatingCityId.getId
              CallBPP.confirmV2 onInitRes.bppUrl confirmBecknReq onInitRes.merchant.id
      else do
        let cancellationReason = "on_init API failure"
            cancelReq = buildCancelReq cancellationReason OnInit
        booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound $ "transactionId:-" <> transactionId)
        errHandlerAction booking cancelReq
    pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
        let cancellationReason = "Confirm Beckn API Call failure"
            cancelReq = buildCancelReq cancellationReason OnConfirm
        errHandlerAction booking cancelReq
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
        let cancellationReason = "External API Call failure, during confirm beckn api"
            cancelReq = buildCancelReq cancellationReason OnConfirm
        errHandlerAction booking cancelReq
      | otherwise = throwM exc

    errHandlerAction booking cancelReq = do
      dCancelRes <- DCancel.cancel booking Nothing cancelReq SBCR.ByApplication
      void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes Nothing

    buildCancelReq cancellationReason reasonStage =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode cancellationReason,
          reasonStage,
          additionalInfo = Nothing,
          reallocate = Nothing,
          blockOnCancellationRate = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BookingId-" <> id
