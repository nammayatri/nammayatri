{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Confirm (API, handler) where

import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Servant hiding (throwError)
import qualified SharedLogic.CallBAP as BP
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as QPerson

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Confirm.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Confirm.ConfirmReq ->
  ByteString ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  (dConfirmReq, callbackUrl, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.confirmReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "Confirm APIV2 Flow" "Reached"
        dConfirmReq <- ACL.buildConfirmReqV2 reqV2
        let context = reqV2.confirmReqContext
        callbackUrl <- Utils.getContextBapUri context
        bppUri <- Utils.getContextBppUri context
        messageId <- Utils.getMessageId context
        bapId <- Utils.getContextBapId context
        city <- Utils.getContextCity context
        country <- Utils.getContextCountry context
        pure (dConfirmReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Confirm APIV1 Flow" "Reached"
        dConfirmReq <- ACL.buildConfirmReq reqV1
        pure (dConfirmReq, reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)

  Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
    now <- getCurrentTime
    (transporter, eitherQuote) <- DConfirm.validateRequest subscriber transporterId dConfirmReq now
    fork "confirm" $ do
      isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
      Redis.whenWithLockRedis (confirmProcessingLockKey dConfirmReq.bookingId.getId) 60 $ do
        dConfirmRes <- DConfirm.handler transporter dConfirmReq eitherQuote
        case dConfirmRes.booking.bookingType of
          DBooking.NormalBooking -> do
            ride <- dConfirmRes.ride & fromMaybeM (RideNotFound dConfirmRes.booking.id.getId)
            driverId <- dConfirmRes.driverId & fromMaybeM (InvalidRequest "driverId Not Found for Normal Booking")
            --driverQuote <- QDQ.findById (Id dConfirmRes.booking.quoteId) >>= fromMaybeM (QuoteNotFound dConfirmRes.booking.quoteId)
            driver <- runInReplica $ QPerson.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
            -- driver <- QPerson.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
            let booking = dConfirmRes.booking
            fork "on_confirm/on_update" $ do
              handle (errHandler dConfirmRes transporter (Just driver)) $ do
                if isBecknSpecVersion2
                  then do
                    context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
                    onConfirmMessage <- ACL.buildOnConfirmMessageV2 dConfirmRes
                    void $ BP.callOnConfirmV2 dConfirmRes.transporter context onConfirmMessage
                    void $ BP.sendRideAssignedUpdateToBAP dConfirmRes.booking ride
                  else do
                    context <- buildTaxiContext Context.CONFIRM msgId txnId bapId callbackUrl bppId bppUri city country False
                    onConfirmMessage <- ACL.buildOnConfirmMessage dConfirmRes
                    void $ BP.callOnConfirm dConfirmRes.transporter context onConfirmMessage
                    void $ BP.sendRideAssignedUpdateToBAP dConfirmRes.booking ride
            DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = transporterId, driverId = Id driverId}
          DBooking.SpecialZoneBooking -> do
            fork "on_confirm/on_update" $ do
              handle (errHandler' dConfirmRes transporter) $ do
                if isBecknSpecVersion2
                  then do
                    context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
                    onConfirmMessage <- ACL.buildOnConfirmMessageV2 dConfirmRes
                    void $ BP.callOnConfirmV2 dConfirmRes.transporter context onConfirmMessage
                  else do
                    context <- buildTaxiContext Context.CONFIRM msgId txnId bapId callbackUrl bppId bppUri city country False
                    onConfirmMessage <- ACL.buildOnConfirmMessage dConfirmRes
                    void $ BP.callOnConfirm dConfirmRes.transporter context onConfirmMessage
  pure Ack
  where
    errHandler dConfirmRes transporter driver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | otherwise = throwM exc

    errHandler' dConfirmRes transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking Nothing transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking Nothing transporter
      | otherwise = throwM exc

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id

confirmProcessingLockKey :: Text -> Text
confirmProcessingLockKey id = "Driver:Confirm:Processing:BookingId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Confirm.ConfirmReq Confirm.ConfirmReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
