{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnInit where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Action.Beckn.FRFS.Common (DFareBreakUp)
import qualified Domain.Types.FRFSTicketBooking as FTBooking
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
import SharedLogic.CreateFareForMultiModal (createVendorSplitFromBookings)
import SharedLogic.FRFSUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payment as Payment

data DOnInit = DOnInit
  { providerId :: Text,
    totalPrice :: Price,
    totalQuantity :: Int,
    totalChildTicketQuantity :: Maybe Int,
    fareBreakUp :: [DFareBreakUp],
    bppItemId :: Text,
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    bankAccNum :: Text,
    bankCode :: Text,
    bppOrderId :: Maybe Text
  }

validateRequest :: (EsqDBReplicaFlow m r, BeamFlow m r) => DOnInit -> m (Merchant.Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnInit {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QFRFSTicketBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onInit ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  DOnInit ->
  Merchant.Merchant ->
  FTBooking.FRFSTicketBooking ->
  m ()
onInit onInitReq merchant oldBooking = do
  person <- QP.findById oldBooking.riderId >>= fromMaybeM (PersonNotFound oldBooking.riderId.getId)
  whenJust (onInitReq.validTill) (\validity -> void $ QFRFSTicketBooking.updateValidTillById validity oldBooking.id)
  void $ QFRFSTicketBooking.updatePriceAndQuantityById onInitReq.totalPrice onInitReq.totalQuantity onInitReq.totalChildTicketQuantity oldBooking.id -- Full Ticket Price (Multiplied By Quantity)
  void $ QFRFSTicketBooking.updateBppBankDetailsById (Just onInitReq.bankAccNum) (Just onInitReq.bankCode) oldBooking.id
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId oldBooking.merchantOperatingCityId Nothing >>= fromMaybeM (FRFSConfigNotFound oldBooking.merchantOperatingCityId.getId)
  whenJust onInitReq.bppOrderId (\bppOrderId -> void $ QFRFSTicketBooking.updateBPPOrderIdById (Just bppOrderId) oldBooking.id)
  let booking = oldBooking {FTBooking.price = onInitReq.totalPrice, FTBooking.journeyOnInitDone = Just True}
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  logInfo $ "onInit journeyId" <> show booking.journeyId
  case booking.journeyId of
    Just journeyId -> do
      logInfo $ "Booking with journeyId" <> show journeyId
      QFRFSTicketBooking.updateOnInitDone (Just True) booking.id
      allJourneyBookings <- QFRFSTicketBooking.findAllByJourneyId (Just journeyId)
      let allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyBookings
      when allLegsOnInitDone $ do
        Redis.withLockRedis (key journeyId) 60 $ do
          let paymentType = Payment.FRFSMultiModalBooking
          (vendorSplitDetails, amount) <- createVendorSplitFromBookings allJourneyBookings merchant.id oldBooking.merchantOperatingCityId paymentType (isMetroTestTransaction && frfsConfig.isFRFSTestingEnabled)
          createPayments allJourneyBookings oldBooking.merchantOperatingCityId oldBooking.merchantId amount person paymentType vendorSplitDetails
    Nothing -> do
      logInfo $ "Booking with journeyId" <> show booking
      let paymentType = getPaymentType booking.vehicleType
      let amount :: HighPrecMoney = if isMetroTestTransaction && frfsConfig.isFRFSTestingEnabled then 1 else booking.price.amount
      createPayments [booking] oldBooking.merchantOperatingCityId oldBooking.merchantId amount person paymentType []
  where
    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking
      Spec.SUBWAY -> Payment.FRFSBooking
    key journeyId = "initJourney-" <> journeyId.getId

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
  m ()
createPayments bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr = do
  ticketBookingPaymentsExist <- mapM (fmap isNothing . QFRFSTicketBookingPayment.findNewTBPByBookingId . (.id)) bookings
  isPaymentOrderProcessed <-
    if and ticketBookingPaymentsExist
      then do
        paymentOrder <- createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr
        return $ isJust paymentOrder
      else do
        updatedPaymentOrder <- JourneyUtils.postMultimodalPaymentUpdateOrderUtil paymentType person merchantId merchantOperatingCityId bookings
        return $ isJust updatedPaymentOrder
  if isPaymentOrderProcessed
    then markBookingApproved `mapM_` bookings
    else do
      markBookingFailed `mapM_` bookings
      throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
  where
    markBookingApproved booking = do
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
      void $ QFRFSTicketBooking.updateFinalPriceById (Just booking.price) booking.id
    markBookingFailed booking = void $ QFRFSTicketBooking.updateStatusById FTBooking.FAILED booking.id
