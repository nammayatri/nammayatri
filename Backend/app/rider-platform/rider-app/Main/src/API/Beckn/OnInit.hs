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
import Domain.Types.CancellationReason
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Booking as QRideB

type API = OnInit.OnInitAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  -- OnInit.OnInitReq ->
  ByteString ->
  FlowHandler AckResponse
onInit _ reqBS = withFlowHandlerBecknAPI $ do
  req <- Common.decodeReq reqBS
  (txnId, mbDOnInitReq) <- case req of
    Right reqV2 -> do
      transactionId <- Common.getTransactionId reqV2.onInitReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        mreq <- TaxiACL.buildOnInitReqV2 reqV2
        return (transactionId, mreq)
    Left reqV1 -> withTransactionIdLogTag reqV1 $ do
      let txnId = fromMaybe "Unknown" $ reqV1.context.transaction_id
      mreq <- TaxiACL.buildOnInitReq reqV1
      return (txnId, mreq)

  Utils.withTransactionIdLogTag txnId $
    whenJust mbDOnInitReq $ \onInitReq ->
      Redis.whenWithLockRedis (onInitLockKey onInitReq.bppBookingId.getId) 60 $
        fork "oninit request processing" $ do
          onInitRes <- DOnInit.onInit onInitReq
          booking <- QRideB.findById onInitRes.bookingId >>= fromMaybeM (BookingDoesNotExist onInitRes.bookingId.getId)
          handle (errHandler booking) $
            void $ withShortRetry $ CallBPP.confirm onInitRes.bppUrl =<< ACL.buildConfirmReq onInitRes
  pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | otherwise = throwM exc

    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BppBookingId-" <> id
