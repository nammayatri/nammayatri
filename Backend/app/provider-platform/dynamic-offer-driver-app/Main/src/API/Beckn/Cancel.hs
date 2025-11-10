{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Cancel (API, handler) where

import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.OnCancel as ACL
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Core.Taxi.API.OnCancel as OnCancel
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OnCancel as OC
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.SearchTryLocker as STL
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Storage.Queries.SearchTry as QST
import Tools.Error
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Cancel.CancelAPIV2

handler :: FlowServer API
handler = cancel

cancel ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Cancel.CancelReqV2 ->
  FlowHandler AckResponse
cancel transporterId subscriber reqV2 = withFlowHandlerBecknAPI do
  (dCancelReq, callbackUrl, bapId, msgId, city, country, txnId, bppId, bppUri) <- do
    transactionId <- Utils.getTransactionId reqV2.cancelReqContext
    Utils.withTransactionIdLogTag transactionId $ do
      logTagInfo "Cancel APIV2 Flow" "Reached"
      dCancelReq <- ACL.buildCancelReqV2 reqV2
      let context = reqV2.cancelReqContext
      callbackUrl <- Utils.getContextBapUri context
      bppUri <- Utils.getContextBppUri context
      messageId <- Utils.getMessageId context
      bapId <- Utils.getContextBapId context
      city <- Utils.getContextCity context
      country <- Utils.getContextCountry context
      pure (dCancelReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)

  logDebug $ "Cancel Request: " <> T.pack (show dCancelReq)
  case dCancelReq of
    DCancel.CancelRide cancelRideReq -> do
      internalEndPointHashMap <- asks (.internalEndPointHashMap)
      merchant <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
      booking <- QRB.findById cancelRideReq.bookingId >>= fromMaybeM (BookingDoesNotExist cancelRideReq.bookingId.getId)
      fork "cancel received pushing ondc logs" do
        void $ pushLogs "cancel" (toJSON reqV2) merchant.id.getId "MOBILITY"
      let vehicleCategory = Utils.mapServiceTierToCategory booking.vehicleServiceTier
      bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
      ttl <- bppConfig.onCancelTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
      context <- ContextV2.buildContextV2 Context.ON_CANCEL Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country (Just ttl)
      let cancelStatus = A.decode . A.encode =<< cancelRideReq.cancelStatus
      case cancelStatus of
        Just Enums.CONFIRM_CANCEL -> do
          Redis.whenWithLockRedis (cancelLockKey cancelRideReq.bookingId.getId) 60 $ do
            (_merchant, _booking) <- DCancel.validateCancelRequest transporterId subscriber cancelRideReq
            mbActiveSearchTry <- QST.findActiveTryByQuoteId _booking.quoteId
            fork ("cancelBooking:" <> cancelRideReq.bookingId.getId) $ do
              (isReallocated, cancellationCharge) <- DCancel.cancel cancelRideReq merchant booking mbActiveSearchTry
              let onCancelBuildReq =
                    OC.DBookingCancelledReqV2
                      { booking = booking,
                        cancellationSource = DBCR.ByUser,
                        cancellationFee = cancellationCharge
                      }
              unless isReallocated $ do
                buildOnCancelMessageV2 <- ACL.buildOnCancelMessageV2 merchant (Just city) (Just country) (show Enums.CANCELLED) (OC.BookingCancelledBuildReqV2 onCancelBuildReq) (Just msgId)
                void $
                  Callback.withCallback merchant "on_cancel" OnCancel.onCancelAPIV2 callbackUrl internalEndPointHashMap (errHandler context) $ do
                    pure buildOnCancelMessageV2
        Just Enums.SOFT_CANCEL -> do
          mbRide <- QRide.findActiveByRBId booking.id
          cancellationCharges <- maybe (return Nothing) (\ride -> DCancel.getCancellationCharges booking ride) mbRide
          void $ case (cancellationCharges, mbRide) of
            (Just priceEntity, Just ride) -> QRide.updateCancellationFeeIfCancelledField (Just priceEntity.amount) ride.id
            _ -> return ()
          let onCancelBuildReq =
                OC.DBookingCancelledReqV2
                  { booking = booking,
                    cancellationSource = DBCR.ByUser,
                    cancellationFee = cancellationCharges
                  }
          buildOnCancelMessageV2 <- ACL.buildOnCancelMessageV2 merchant (Just city) (Just country) (show Enums.SOFT_CANCEL) (OC.BookingCancelledBuildReqV2 onCancelBuildReq) (Just msgId)
          void $
            Callback.withCallback merchant "on_cancel" OnCancel.onCancelAPIV2 callbackUrl internalEndPointHashMap (errHandler context) $ do
              pure buildOnCancelMessageV2
        _ -> throwError $ InvalidRequest "Invalid cancel status"
      return Ack
    DCancel.CancelSearch cancelSearchReq -> do
      searchTry <- DCancel.validateCancelSearchRequest transporterId subscriber cancelSearchReq
      -- Lock Description: This is a Lock held between Driver Respond and Cancel Search, if Driver Respond Quote is OnGoing then the Cancel Search will fail with `DriverAlreadyQuoted`.
      -- Lock Release: Held for 5 seconds once acquired, never released.
      lockAndCall searchTry $ do
        DCancel.cancelSearch transporterId searchTry
      return Ack
  where
    lockAndCall searchTry action = STL.whenSearchTryCancellable searchTry.id action

cancelLockKey :: Text -> Text
cancelLockKey id = "Driver:Cancel:BookingId-" <> id

errHandler :: Spec.Context -> BecknAPIError -> Spec.OnCancelReq
errHandler context (BecknAPIError err) =
  Spec.OnCancelReq
    { onCancelReqContext = context,
      onCancelReqError = Just err',
      onCancelReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
