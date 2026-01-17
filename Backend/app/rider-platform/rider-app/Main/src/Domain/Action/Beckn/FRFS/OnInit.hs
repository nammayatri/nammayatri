{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnInit where

import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingStatus as FTBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JourneyUtils
import Lib.Payment.Storage.Beam.BeamFlow
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.Payment as Payment

data DOnInit = DOnInit
  { providerId :: Text,
    totalPrice :: Price,
    categories :: [DCategorySelect],
    fareBreakUp :: [DFareBreakUp],
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    bankAccNum :: Text,
    bankCode :: Text,
    bppOrderId :: Maybe Text
  }

validateRequest :: (EsqDBReplicaFlow m r, BeamFlow m r) => DOnInit -> m (Merchant.Merchant, FTBooking.FRFSTicketBooking, [DFRFSQuoteCategory.FRFSQuoteCategory])
validateRequest DOnInit {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QFRFSTicketBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking, quoteCategories)

onInit ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    Metrics.HasBAPMetrics m r,
    HasShortDurationRetryCfg r c
  ) =>
  DOnInit ->
  Merchant.Merchant ->
  FTBooking.FRFSTicketBooking ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe Bool ->
  m ()
onInit onInitReq merchant oldBooking quoteCategories mbEnableOffer = do
  Metrics.finishMetrics Metrics.INIT_FRFS merchant.name onInitReq.transactionId oldBooking.merchantOperatingCityId.getId
  person <- QP.findById oldBooking.riderId >>= fromMaybeM (PersonNotFound oldBooking.riderId.getId)
  whenJust (onInitReq.validTill) (\validity -> void $ QFRFSTicketBooking.updateValidTillById validity oldBooking.id)
  let totalPrice = onInitReq.totalPrice
  (updatedQuoteCategories, isFareChanged) <-
    updateQuoteCategoriesWithFinalPrice
      ( mapMaybe
          ( \quoteCategory ->
              find (\category -> category.category == quoteCategory.category) quoteCategories
                <&> \quoteCategory' -> (quoteCategory'.id, quoteCategory.price)
          )
          onInitReq.categories
      )
      quoteCategories
  let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories)

  when (totalPrice /= fareParameters.totalPrice) $ do
    throwError $ CategoriesAndTotalPriceMismatch (show fareParameters.totalPrice) (show totalPrice)

  -- TODO :: Remove Quantity update Booking Table post release of FRFSQuoteCategory
  void $ QFRFSTicketBooking.updateTotalPriceById totalPrice oldBooking.id
  void $ QFRFSTicketBooking.updateIsFareChangedById (Just isFareChanged) oldBooking.id -- Full Ticket Price (Multiplied By Quantity)
  void $ QFRFSTicketBooking.updateBppBankDetailsById (Just onInitReq.bankAccNum) (Just onInitReq.bankCode) oldBooking.id
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId oldBooking.merchantOperatingCityId Nothing >>= fromMaybeM (FRFSConfigNotFound oldBooking.merchantOperatingCityId.getId)
  whenJust onInitReq.bppOrderId (\bppOrderId -> void $ QFRFSTicketBooking.updateBPPOrderIdById (Just bppOrderId) oldBooking.id)
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let booking = oldBooking {FTBooking.totalPrice = totalPrice, FTBooking.journeyOnInitDone = Just True}
  QFRFSTicketBooking.updateOnInitDone (Just True) booking.id
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  (mbJourneyId, allJourneyBookings) <- getAllJourneyFrfsBookings booking

  let allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyBookings
  when allLegsOnInitDone $ do
    Redis.withLockRedis (key (maybe booking.id.getId (.getId) mbJourneyId)) 60 $ do
      let paymentType = getPaymentType (integratedBPPConfig.platformType == DIBC.MULTIMODAL) booking.vehicleType
      (vendorSplitDetails, amount) <- createVendorSplitFromBookings allJourneyBookings merchant.id oldBooking.merchantOperatingCityId paymentType (isMetroTestTransaction && frfsConfig.isFRFSTestingEnabled)
      baskets <- case mbJourneyId of
        Just _ -> do
          Just <$> createBasketFromBookings allJourneyBookings merchant.id oldBooking.merchantOperatingCityId paymentType mbEnableOffer
        Nothing -> return Nothing
      createPayments allJourneyBookings oldBooking.merchantOperatingCityId oldBooking.merchantId amount person paymentType vendorSplitDetails baskets mbEnableOffer mbJourneyId
  where
    key journeyId = "initJourney-" <> journeyId

createPayments ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id DMOC.MerchantOperatingCity ->
  Id Merchant.Merchant ->
  HighPrecMoney ->
  DP.Person ->
  Payment.PaymentServiceType ->
  [Payment.VendorSplitDetails] ->
  Maybe [Payment.Basket] ->
  Maybe Bool ->
  Maybe (Id DJ.Journey) ->
  m ()
createPayments bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket mbEnableOffer mbJourneyId = do
  ticketBookingPaymentsExist <- mapM (fmap isNothing . QFRFSTicketBookingPayment.findTicketBookingPayment) bookings
  mbPaymentOrder <-
    if and ticketBookingPaymentsExist
      then do
        paymentOrder <- createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket
        return paymentOrder
      else do
        updatedPaymentOrder <- JourneyUtils.postMultimodalPaymentUpdateOrderUtil paymentType person merchantId merchantOperatingCityId bookings mbEnableOffer
        return updatedPaymentOrder
  case mbPaymentOrder of
    Just paymentOrder -> mapM_ (markBookingApproved paymentOrder) bookings
    Nothing -> do
      markBookingFailed `mapM_` bookings
      throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
  where
    markBookingApproved paymentOrder booking = do
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
      whenJust mbJourneyId $ \journeyId -> do
        void $ QJourney.updatePaymentOrderShortId (Just paymentOrder.shortId) Nothing journeyId
    markBookingFailed booking = void $ QFRFSTicketBooking.updateStatusById FTBooking.FAILED booking.id
