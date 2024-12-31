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
import BecknV2.FRFS.Utils
import Domain.Action.Beckn.FRFS.Common as Common
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import SharedLogic.FRFSUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QP
import qualified Tools.Payment as Payment

validateRequest :: Common.DOnInit -> Flow (Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnInit {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QFRFSTicketBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onInit ::
  Common.DOnInit ->
  Merchant ->
  FTBooking.FRFSTicketBooking ->
  Flow ()
onInit onInitReq merchant booking_ = do
  person <- QP.findById booking_.riderId >>= fromMaybeM (PersonNotFound booking_.riderId.getId)
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  personEmail <- mapM decrypt person.email
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking_.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  whenJust (onInitReq.validTill) (\validity -> void $ QFRFSTicketBooking.updateValidTillById validity booking_.id)
  void $ QFRFSTicketBooking.updatePriceById onInitReq.totalPrice booking_.id
  void $ QFRFSTicketBooking.updateBppBankDetailsById (Just onInitReq.bankAccNum) (Just onInitReq.bankCode) booking_.id
  let booking = booking_ {FTBooking.price = onInitReq.totalPrice}

  orderShortId <- generateShortId
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let updatedOrderShortId = bool (orderShortId.getShortId) ("test-" <> orderShortId.getShortId) isMetroTestTransaction
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  orderId <- generateGUID
  ticketBookingPaymentId <- generateGUID
  now <- getCurrentTime
  paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
  paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall person merchantOperatingCity.id booking)
  if booking.price.amount == HighPrecMoney 0.0
    then do
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.CONFIRMING booking.id
      void $ QFRFSTicketBooking.updateFinalPriceById (Just onInitReq.totalPrice) booking.id
      void $ handleConfirmProcess merchant merchantOperatingCity bapConfig booking person paymentStatusResp now
    else do
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

      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = orderId.getId,
                orderShortId = updatedOrderShortId,
                amount = booking.price.amount,
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
                splitSettlementDetails = Nothing
              }
      let mocId = booking.merchantOperatingCityId
          commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchant.id
          createOrderCall = Payment.createOrder merchant.id mocId Nothing (getPaymentType booking.vehicleType)
      mCreateOrderRes <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast mocId) commonPersonId createOrderReq createOrderCall
      case mCreateOrderRes of
        Just _ -> do
          void $ QFRFSTicketBookingPayment.create ticketBookingPayment
          void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId FTBooking.APPROVED booking.id
          void $ QFRFSTicketBooking.updateFinalPriceById (Just onInitReq.totalPrice) booking.id
        Nothing -> do
          void $ QFRFSTicketBooking.updateStatusById FTBooking.FAILED booking.id
          throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
  where
    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking

    orderStatusCall person _merchantOperatingCityId currentBooking = Payment.orderStatus merchant.id person.merchantOperatingCityId Nothing (getPaymentType currentBooking.vehicleType)
