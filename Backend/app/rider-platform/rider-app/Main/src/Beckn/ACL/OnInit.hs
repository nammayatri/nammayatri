{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit (buildOnInitReqV2) where

import qualified Beckn.ACL.Cancel as CancelACL
import Beckn.ACL.Common
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Types.Booking (BPPBooking, Booking)
import Domain.Types.CancellationReason
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRideB

buildOnInitReqV2 ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["_version" ::: Text, "internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl]
  ) =>
  Spec.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitReqV2 req = do
  ContextV2.validateContext Context.ON_INIT $ req.onInitReqContext
  handleErrorV2 req $ \_message ->
    case parsedData of
      Left err -> do
        logTagError "on_init req" $ "on_init error: " <> show err
        pure Nothing
      Right (bookingId, bppBookingId, estimatedFare, estimatedTotalFare) -> do
        validatePrices estimatedFare estimatedTotalFare
        -- if we get here, the discount >= 0
        let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
        return $
          Just $
            DOnInit.OnInitReq
              { estimatedFare = Money estimatedFare,
                estimatedTotalFare = Money estimatedTotalFare,
                discount = Money <$> discount,
                paymentUrl = Nothing, -- TODO check with ONDC
                ..
              }
  where
    parsedData :: Either Text (Id Booking, Id BPPBooking, Int, Int)
    parsedData = do
      order <- maybe (Left "Invalid Order") (Right . (.confirmReqMessageOrder)) req.onInitReqMessage

      bookingIdText <-
        (fmap UUID.toText req.onInitReqContext.contextMessageId)
          & maybe (Left "Invalid messageId") Right
      let bookingId = Id bookingIdText

      bppBookingIdText <-
        order.orderId
          & maybe (Left "Invalid OrderId") Right
      let bppBookingId = Id bppBookingIdText

      estimatedFare <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceValue)
          >>= parseInt
          & maybe (Left "Invalid Price") Right

      estimatedTotalFare <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceOfferedValue)
          >>= parseInt
          & maybe (Left "Invalid Offered Price") Right

      Right (bookingId, bppBookingId, estimatedFare, estimatedTotalFare)

    parseInt :: Text -> Maybe Int
    parseInt = readMaybe . T.unpack

handleErrorV2 ::
  (EncFlow m r, Esq.EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl]) =>
  Spec.OnInitReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnInit.OnInitReq)) ->
  m (Maybe DOnInit.OnInitReq)
handleErrorV2 req action =
  case req.onInitReqError of
    Nothing -> req.onInitReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      bookingIdText <- (fmap UUID.toText req.onInitReqContext.contextMessageId) & fromMaybeM (InvalidRequest "Missing messageId in context")
      let bookingId = Id bookingIdText
          cancelReq = buildCancelReq err.errorMessage
      booking <- QRideB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
      void $ CallBPP.cancelV2 dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes
      pure Nothing

buildCancelReq :: Maybe Text -> DCancel.CancelReq
buildCancelReq mbErrorMessage =
  DCancel.CancelReq
    { reasonCode = CancellationReasonCode "BPP cancelled: on_init error",
      reasonStage = OnConfirm,
      additionalInfo = mbErrorMessage
    }
