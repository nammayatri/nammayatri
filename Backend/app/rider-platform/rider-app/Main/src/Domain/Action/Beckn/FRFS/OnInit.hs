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
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import Lib.Payment.Storage.Beam.BeamFlow
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QP
import qualified Tools.Payment as Payment

data DOnInit = DOnInit
  { providerId :: Text,
    totalPrice :: Price,
    fareBreakUp :: [DFareBreakUp],
    bppItemId :: Text,
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    bankAccNum :: Text,
    bankCode :: Text
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
onInit onInitReq merchant booking_ = do
  person <- QP.findById booking_.riderId >>= fromMaybeM (PersonNotFound booking_.riderId.getId)
  whenJust (onInitReq.validTill) (\validity -> void $ QFRFSTicketBooking.updateValidTillById validity booking_.id)
  void $ QFRFSTicketBooking.updatePriceById onInitReq.totalPrice booking_.id
  void $ QFRFSTicketBooking.updateBppBankDetailsById (Just onInitReq.bankAccNum) (Just onInitReq.bankCode) booking_.id
  let booking = booking_ {FTBooking.price = onInitReq.totalPrice, FTBooking.journeyOnInitDone = Just True}

  case booking.journeyId of
    Just journeyId -> do
      QFRFSTicketBooking.updateOnInitDone (Just True) booking.id
      allJourneyBookings <- QFRFSTicketBooking.findAllByJourneyId (Just journeyId)
      let allLegsOnInitDone = all (\b -> b.journeyOnInitDone == Just True) allJourneyBookings
      when allLegsOnInitDone $ do
        Redis.withLockRedis (key journeyId) 60 $ do
          (orderId, orderShortId) <- getPaymentIds
          ticketBookingPayments <- processPayments orderId `mapM` allJourneyBookings
          let amount = sum $ allJourneyBookings <&> (.price.amount)
          let paymentType = Payment.FRFSMultiModalBooking
          mCreateOrderRes <- createPayments booking.merchantOperatingCityId merchant.id orderId orderShortId amount person paymentType
          case mCreateOrderRes of
            Just _ -> do
              let bookingAndPayments = zip ticketBookingPayments allJourneyBookings
              markBookingApproved `mapM_` bookingAndPayments
            Nothing -> do
              markBookingFailed `mapM_` allJourneyBookings
              throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
    Nothing -> do
      (orderId, orderShortId) <- getPaymentIds
      ticketBookingPayment <- processPayments orderId booking
      let paymentType = getPaymentType booking.vehicleType
      mCreateOrderRes <- createPayments booking.merchantOperatingCityId merchant.id orderId orderShortId booking.price.amount person paymentType
      case mCreateOrderRes of
        Just _ -> do
          markBookingApproved (ticketBookingPayment, booking)
        Nothing -> do
          markBookingFailed booking
          throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
  where
    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking
      Spec.SUBWAY -> Payment.FRFSBooking

    key journeyId = "initJourney-" <> journeyId.getId

    getPaymentIds = do
      orderShortId <- generateShortId
      orderId <- generateGUID
      isMetroTestTransaction <- asks (.isMetroTestTransaction)
      let updatedOrderShortId = bool (orderShortId.getShortId) ("test-" <> orderShortId.getShortId) isMetroTestTransaction
      return (orderId, updatedOrderShortId)

    markBookingApproved (ticketBookingPayment, booking) = do
      let price = if booking.id == booking_.id then onInitReq.totalPrice else booking.price
      void $ QFRFSTicketBookingPayment.create ticketBookingPayment
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
      void $ QFRFSTicketBooking.updateFinalPriceById (Just price) booking.id

    markBookingFailed booking = void $ QFRFSTicketBooking.updateStatusById FTBooking.FAILED booking.id

processPayments ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  Id PaymentOrder.PaymentOrder ->
  FTBooking.FRFSTicketBooking ->
  m DFRFSTicketBookingPayment.FRFSTicketBookingPayment
processPayments orderId booking = do
  ticketBookingPaymentId <- generateGUID
  now <- getCurrentTime
  let ticketBookingPayment =
        DFRFSTicketBookingPayment.FRFSTicketBookingPayment
          { frfsTicketBookingId = booking.id,
            id = ticketBookingPaymentId,
            status = DFRFSTicketBookingPayment.PENDING,
            merchantId = Just booking.merchantId,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            paymentOrderId = orderId
          }
  return ticketBookingPayment

createPayments ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Merchant.Merchant ->
  Id PaymentOrder.PaymentOrder ->
  Text ->
  HighPrecMoney ->
  DP.Person ->
  Payment.PaymentServiceType ->
  m (Maybe Payment.CreateOrderResp)
createPayments merchantOperatingCityId merchantId orderId orderShortId amount person paymentType = do
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  personEmail <- mapM decrypt person.email
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentType
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = orderId.getId,
            orderShortId = orderShortId,
            amount = amount,
            customerId = person.id.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
            splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled amount []
          }
  let mocId = merchantOperatingCityId
      commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
      createOrderCall = Payment.createOrder merchantId mocId Nothing paymentType
  DPayment.createOrderService commonMerchantId (Just $ cast mocId) commonPersonId createOrderReq createOrderCall
