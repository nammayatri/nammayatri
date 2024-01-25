{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnInit where

import Domain.Action.Beckn.FRFS.Common (DFareBreakUp)
import qualified Domain.Types.FRFSTicketBooking as Init
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QP
import qualified Tools.Payment as Payment

data DOnInit = DOnInit
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    fareBreakUp :: [DFareBreakUp],
    bppItemId :: Text,
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text
  }

validateRequest :: DOnInit -> Flow (Merchant, Init.FRFSTicketBooking)
validateRequest DOnInit {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  booking <- runInReplica $ QFRFSTicketBooking.findById (Id messageId) >>= fromMaybeM (BookingDoesNotExist messageId)
  merchantId <- booking.merchantId & fromMaybeM (InternalError "MerchantId not found in booking") -- TODO: Make merchantId required
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onInit ::
  DOnInit ->
  Merchant ->
  Init.FRFSTicketBooking ->
  Flow ()
onInit onInitReq merchant booking = do
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  personEmail <- mapM decrypt person.email

  whenJust (onInitReq.validTill) (\validity -> void $ QFRFSTicketBooking.updateValidTillById validity booking.id)

  orderShortId <- generateShortId
  orderId <- generateGUID
  ticketBookingPaymentId <- generateGUID
  now <- getCurrentTime

  let ticketBookingPayment =
        DFRFSTicketBookingPayment.FRFSTicketBookingPayment
          { frfsTicketBookingId = booking.id,
            id = ticketBookingPaymentId,
            status = DFRFSTicketBookingPayment.PENDING,
            merchantId = Just merchant.id,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            paymentOrderId = orderId
          }

  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = orderId.getId,
            orderShortId = orderShortId.getShortId,
            amount = booking.price,
            customerId = person.id.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing
          }

  let commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchant.id
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
      createOrderCall = Payment.createOrder merchant.id
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just _ -> do
      void $ QFRFSTicketBookingPayment.create ticketBookingPayment
      void $ QFRFSTicketBooking.updateBPPOrderIdAndStatusById booking.bppOrderId Init.APPROVED booking.id
    Nothing -> do
      void $ QFRFSTicketBooking.updateStatusById Init.FAILED booking.id
      throwError $ InternalError "Failed to create order with Euler after on_int in FRFS"
