{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit (buildOnInitReqV2) where

import Beckn.ACL.Common as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Domain.Types.Booking (BPPBooking, Booking)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.DecimalValue as DecimalValue
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnInitReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m,
    CacheFlow m r
  ) =>
  Spec.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitReqV2 req = do
  ContextV2.validateContext Context.ON_INIT $ req.onInitReqContext
  handleErrorV2 req $ \message -> do
    let order = message.confirmReqMessageOrder
    fareParamsQuotationBreakup <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "Missing Quote Breakups.")
    let fareBreakups = mapMaybe ACL.mkDFareBreakup fareParamsQuotationBreakup
        commission = extractCommissionFromPaymentTags order.orderPayments
    case parsedData of
      Left err -> throwError . InvalidBecknSchema $ "on_init req," <> "on_init error: " <> show err
      Right (bookingId, bppBookingId, estimatedFare, paymentId, currency) -> do
        return $
          Just $
            DOnInit.OnInitReq
              { estimatedFare = Utils.decimalValueToPrice currency estimatedFare,
                discount = Nothing, -- TODO : replace when actual discount logic is implemented
                paymentUrl = Nothing, -- TODO check with ONDC
                commission = commission,
                ..
              }
  where
    parsedData :: Either Text (Id Booking, Maybe (Id BPPBooking), DecimalValue.DecimalValue, Maybe Text, Currency)
    parsedData = do
      order <- maybe (Left "Invalid Order") (Right . (.confirmReqMessageOrder)) req.onInitReqMessage

      bookingIdText <-
        (fmap UUID.toText req.onInitReqContext.contextMessageId)
          & maybe (Left "Invalid messageId") Right
      let bookingId = Id bookingIdText

      let bppBookingIdText = order.orderId
      let bppBookingId = Id <$> bppBookingIdText

      estimatedFare <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceValue)
          >>= parseDecimalValue
          & maybe (Left "Invalid Price") Right

      paymentId <-
        order.orderPayments
          >>= listToMaybe
          >>= (.paymentId)
          & Right

      currency <-
        order.orderQuote
          >>= (.quotationPrice)
          >>= (.priceCurrency)
          >>= (readMaybe . T.unpack)
          & maybe (Left "Invalid Currency") Right

      Right (bookingId, bppBookingId, estimatedFare, paymentId, currency)

    parseDecimalValue :: Text -> Maybe DecimalValue.DecimalValue
    parseDecimalValue = DecimalValue.valueFromString

    extractCommissionFromPaymentTags :: Maybe [Spec.Payment] -> Maybe Common.HighPrecMoney
    extractCommissionFromPaymentTags mbPayments = do
      payments <- mbPayments
      payment <- listToMaybe payments
      paymentTags <- payment.paymentTags
      commissionText <- ACL.getTagV2' Tags.SETTLEMENT_DETAILS Tags.COMMISSION (Just paymentTags)
      Common.highPrecMoneyFromText commissionText

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnInitReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnInit.OnInitReq)) ->
  m (Maybe DOnInit.OnInitReq)
handleErrorV2 req action =
  case req.onInitReqError of
    Nothing -> req.onInitReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing
