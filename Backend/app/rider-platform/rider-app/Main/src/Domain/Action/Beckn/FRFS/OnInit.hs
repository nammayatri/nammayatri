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
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified Lib.Payment.Domain.Action as DPayment
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..))
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
    bppOrderId :: Maybe Text,
    bppPaymentId :: Maybe Text
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
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    Finance.HasActorInfo m r
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
  frfsConfig <- getConfig (FRFSConfigDimensions {merchantOperatingCityId = oldBooking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (FRFSConfigNotFound oldBooking.merchantOperatingCityId.getId)
  whenJust onInitReq.bppOrderId (\bppOrderId -> void $ QFRFSTicketBooking.updateBPPOrderIdById (Just bppOrderId) oldBooking.id)
  whenJust onInitReq.bppPaymentId (\bppPaymentId -> void $ QFRFSTicketBooking.updateBPPPaymentIdById (Just bppPaymentId) oldBooking.id)
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let booking = oldBooking {FTBooking.totalPrice = totalPrice, FTBooking.journeyOnInitDone = Just True}
  QFRFSTicketBooking.updateOnInitDone (Just True) booking.id
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  booking' <- if isFareChanged then dropPassOverrideOnFareChange booking else pure booking
  (mbJourneyId, allJourneyBookings) <- getAllJourneyFrfsBookings booking'

  let allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyBookings
      -- A fully covered leg is already CONFIRMING with the BPP and has no order of its own. It
      -- counts towards allLegsOnInitDone, but it must stay out of the payment list: createPayments
      -- would write it a FRFSTicketBookingPayment row and markBookingApproved would drag it back
      -- from CONFIRMING to APPROVED, so payment success would confirm it with the BPP a second time.
      payableBookings = filter (not . FRFSPassOverride.isFullyPassCovered . (.overriddenAmount)) allJourneyBookings
  when (allLegsOnInitDone && not (null payableBookings)) $ do
    Redis.withLockRedis (key (maybe booking.id.getId (.getId) mbJourneyId)) 60 $ do
      let paymentType = getPaymentType (integratedBPPConfig.platformType == DIBC.MULTIMODAL) booking.vehicleType
      (vendorSplitDetails, amount) <- createVendorSplitFromBookings payableBookings merchant.id oldBooking.merchantOperatingCityId paymentType (isMetroTestTransaction && frfsConfig.isFRFSTestingEnabled)
      baskets <- createBasketFromBookings payableBookings merchant.id oldBooking.merchantOperatingCityId paymentType mbEnableOffer
      createPayments payableBookings oldBooking.merchantOperatingCityId oldBooking.merchantId amount person paymentType vendorSplitDetails baskets mbEnableOffer mbJourneyId
  where
    key journeyId = "initJourney-" <> journeyId

-- A fare change under a pass-covered booking has never been observed (is_fare_changed has never
-- been true across ~30M bookings). Rather than guess a new discount from a fare the rider was
-- never shown, give the trip back and let the booking settle at the full fare -- the status
-- response drops overrideType/overriddenTotalPrice, so the client can say so.
dropPassOverrideOnFareChange ::
  (CacheFlow m r, EsqDBFlow m r) =>
  FTBooking.FRFSTicketBooking ->
  m FTBooking.FRFSTicketBooking
dropPassOverrideOnFareChange booking = case booking.overrideAppliedEntityId of
  Nothing -> pure booking
  Just entityId -> do
    logError $ "OnInit:dropPassOverrideOnFareChange fare changed under a pass-covered booking, dropping override bookingId=" <> booking.id.getId
    -- Clear only once the trip is actually back: overrideAppliedEntityId is the only thing naming
    -- which pass paid. Keeping it on failure is not free -- see .cursor/docs/20-frfs-pass-fare-override.md
    released <- withTryCatch "onInit:dropOverrideReleaseTrip" (FRFSPassOverride.refundPassOverrideTrip booking.searchId (Id entityId))
    case released of
      Left err -> do
        logError $ "OnInit:dropPassOverrideOnFareChange trip release failed, keeping override fields for retry bookingId=" <> booking.id.getId <> " err=" <> show err
        pure booking
      Right _ -> do
        QFRFSTicketBooking.updatePassOverrideById Nothing Nothing Nothing booking.id
        pure booking {FTBooking.overrideType = Nothing, FTBooking.overriddenAmount = Nothing, FTBooking.overrideAppliedEntityId = Nothing}

createPayments ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    Finance.HasActorInfo m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id DMOC.MerchantOperatingCity ->
  Id Merchant.Merchant ->
  HighPrecMoney ->
  DP.Person ->
  Payment.PaymentServiceType ->
  [Payment.VendorSplitDetails] ->
  [Payment.Basket] ->
  Maybe Bool ->
  Maybe (Id DJ.Journey) ->
  m ()
createPayments bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket mbEnableOffer mbJourneyId = do
  ticketBookingPaymentsExist <- mapM (fmap isNothing . QFRFSTicketBookingPayment.findTicketBookingPayment) bookings
  let isMockPayment = all (\booking -> fromMaybe False booking.isMockPayment) bookings
  mbPaymentOrder <-
    if and ticketBookingPaymentsExist
      then do
        paymentOrder <- createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket isMockPayment
        return paymentOrder
      else do
        updatedPaymentOrder <- JourneyUtils.postMultimodalPaymentUpdateOrderUtil paymentType person merchantId merchantOperatingCityId bookings mbEnableOffer isMockPayment
        return updatedPaymentOrder
  case mbPaymentOrder of
    Just paymentOrder -> mapM_ (markBookingApproved paymentOrder) bookings
    Nothing
      -- NOT how pass-covered bookings get approved: those are confirmed directly in
      -- postFrfsQuoteV2ConfirmUtil's isFullyPassCovered branch, which never calls init, so they
      -- never reach on_init at all -- and inside a journey payableBookings filters them out
      -- above. This is a safety net for a genuinely zero-fare booking, and insurance if covered
      -- bookings are ever routed through init. Do not "fix" the covered flow by editing it.
      | amount <= 0 -> mapM_ markBookingPaidWithoutOrder bookings
      | otherwise -> do
        markBookingFailed `mapM_` bookings
        throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
  where
    markBookingApproved paymentOrder booking = do
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
      whenJust mbJourneyId $ \journeyId -> do
        isTestTransaction <- asks (.isMetroTestTransaction)
        let updatedOrderShortId = DPayment.updateShortId (Just paymentType) isTestTransaction paymentOrder.shortId.getShortId
        void $ QJourney.updatePaymentOrderShortId (Just $ ShortId updatedOrderShortId) Nothing journeyId
    markBookingFailed booking = do
      void $ QFRFSTicketBooking.updateStatusById FTBooking.FAILED booking.id
      void $ withTryCatch "onInit:releaseTrip" (FRFSPassOverride.releasePassOverrideTripOnFailure booking.searchId booking.overrideAppliedEntityId)

    markBookingPaidWithoutOrder booking =
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
