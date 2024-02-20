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
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.BecknConfig as QBC

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Confirm.ConfirmAPIV2

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Confirm.ConfirmReqV2 ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.confirmReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "Confirm APIV2 Flow" "Reached"
    let context = reqV2.confirmReqContext
        bppId = context.contextBppId
        txnId = Just transactionId
    bapId <- Utils.getContextBapId context
    callbackUrl <- Utils.getContextBapUri context
    bppUri <- Utils.getContextBppUri context
    msgId <- Utils.getMessageId context
    city <- Utils.getContextCity context
    country <- Utils.getContextCountry context
    isValueAddNP <- CQVAN.isValueAddNP bapId
    dConfirmReq <- ACL.buildConfirmReqV2 reqV2 isValueAddNP

    Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
      now <- getCurrentTime
      (transporter, eitherQuote) <- DConfirm.validateRequest subscriber transporterId dConfirmReq now
      fork "confirm" $ do
        Redis.whenWithLockRedis (confirmProcessingLockKey dConfirmReq.bookingId.getId) 60 $ do
          dConfirmRes <- DConfirm.handler transporter dConfirmReq eitherQuote
          case dConfirmRes.rideInfo of
            Just rideInfo' -> do
              fork "on_confirm/on_update" $ do
                handle (errHandler dConfirmRes transporter (Just rideInfo'.driver)) $ do
                  -- We assign driver post `on_confirm`, so removing `rideInfo` details fron `dConfirmRes`.
                  let dConfirmRes' = dConfirmRes {DConfirm.rideInfo = Nothing}
                  callOnConfirm dConfirmRes' msgId txnId bapId callbackUrl bppId bppUri city country
                  void $ BP.sendRideAssignedUpdateToBAP dConfirmRes.booking rideInfo'.ride rideInfo'.driver rideInfo'.vehicle
            Nothing -> do
              fork "on_confirm/on_update" $ do
                handle (errHandler dConfirmRes transporter Nothing) $ do
                  callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country
    pure Ack
  where
    errHandler dConfirmRes transporter mbDriver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | otherwise = throwM exc

    callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country = do
      context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
      let vehicleCategory = Utils.mapVariantToVehicle dConfirmRes.vehicleVariant
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle (Just dConfirmRes.transporter.id) (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
      onConfirmMessage <- ACL.buildOnConfirmMessageV2 dConfirmRes becknConfig
      void $ BP.callOnConfirmV2 dConfirmRes.transporter context onConfirmMessage

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id

confirmProcessingLockKey :: Text -> Text
confirmProcessingLockKey id = "Driver:Confirm:Processing:BookingId-" <> id
