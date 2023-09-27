{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Payment where

import Domain.Types.DriverFee
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import SharedLogic.DriverFee (roundToHalf)
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

data MandateOrder = MandateOrder
  { maxAmount :: HighPrecMoney,
    _type :: MandateType,
    frequency :: MandateFrequency,
    mandateStartDate :: Text,
    mandateEndDate :: Text,
    planRegistrationAmount :: HighPrecMoney
  }

createOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  ([DriverFee], [DriverFee]) ->
  Maybe MandateOrder ->
  INV.InvoicePaymentMode ->
  Maybe (Id INV.Invoice, Text) ->
  m (CreateOrderResp, Id DOrder.PaymentOrder)
createOrder (driverId, merchantId) (driverFees, driverFeesToAddOnExpiry) mbMandateOrder invoicePaymentMode existingInvoice = do
  mapM_ (\driverFee -> when (driverFee.status `elem` [CLEARED, EXEMPTED, COLLECTED_CASH]) $ throwError (DriverFeeAlreadySettled driverFee.id.getId)) driverFees
  mapM_ (\driverFee -> when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse driverFee.id.getId)) driverFees
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound $ getId driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  genInvoiceId <- generateGUID
  genShortInvoiceId <- generateShortId
  now <- getCurrentTime
  let driverEmail = fromMaybe "test@juspay.in" driver.email
      (invoiceId, invoiceShortId) = fromMaybe (genInvoiceId, genShortInvoiceId.getShortId) existingInvoice
      amount = sum $ (\pendingFees -> roundToHalf (fromIntegral pendingFees.govtCharges + pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst)) <$> driverFees
      registrationFee = find (\df -> df.feeType == MANDATE_REGISTRATION) driverFees
  case (amount <= 0, registrationFee, mbMandateOrder) of
    (True, Just registrationFee', Just mandateOrder) -> do
      uuid <- generateGUID
      let newDriverFee = replicateDriverFeeWithAmount registrationFee' mandateOrder.planRegistrationAmount now uuid
      QDF.updateStatus CLEARED registrationFee'.id now
      QIN.updateInvoiceStatusByDriverFeeIds INV.INACTIVE [registrationFee'.id]
      QDF.create newDriverFee
      createOrder (driverId, merchantId) ([newDriverFee], []) mbMandateOrder invoicePaymentMode Nothing
    (True, Nothing, Nothing) -> throwError $ InternalError "amount should be greater than 0"
    (_, _, _) -> do
      let invoices = mkInvoiceAgainstDriverFee invoiceId.getId invoiceShortId now (mbMandateOrder <&> (.maxAmount)) invoicePaymentMode <$> driverFees
      unless (isJust existingInvoice) $ QIN.createMany invoices
      let createOrderReq =
            CreateOrderReq
              { orderId = invoiceId.getId,
                orderShortId = invoiceShortId,
                amount = amount,
                customerId = driver.id.getId,
                customerEmail = driverEmail,
                customerPhone = driverPhone,
                customerFirstName = Just driver.firstName,
                customerLastName = driver.lastName,
                createMandate = mbMandateOrder <&> (._type),
                mandateMaxAmount = mbMandateOrder <&> (.maxAmount),
                mandateFrequency = mbMandateOrder <&> (.frequency),
                mandateEndDate = mbMandateOrder <&> (.mandateEndDate),
                mandateStartDate = mbMandateOrder <&> (.mandateStartDate)
              }
      let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
          commonPersonId = cast @DP.Person @DPayment.Person driver.id
          createOrderCall = Payment.createOrder merchantId -- api call
      mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
      case mCreateOrderRes of
        Just createOrderRes -> return (createOrderRes, cast invoiceId)
        Nothing -> do
          QIN.updateInvoiceStatusByInvoiceId INV.EXPIRED invoiceId
          createOrder (driverId, merchantId) (driverFees <> driverFeesToAddOnExpiry, []) mbMandateOrder invoicePaymentMode Nothing -- call same function with no existing order
  where
    replicateDriverFeeWithAmount oldDriverFee amount now id =
      DriverFee
        { id = id,
          merchantId = oldDriverFee.merchantId,
          payBy = now,
          status = oldDriverFee.status,
          numRides = 0,
          createdAt = now,
          updatedAt = now,
          platformFee = PlatformFee {fee = amount, cgst = oldDriverFee.platformFee.cgst, sgst = oldDriverFee.platformFee.sgst},
          totalEarnings = 0,
          feeType = MANDATE_REGISTRATION,
          govtCharges = 0,
          startTime = now,
          endTime = now,
          collectedBy = Nothing,
          driverId = oldDriverFee.driverId,
          offerId = Nothing,
          planOfferTitle = Nothing,
          autopayPaymentStage = Nothing,
          stageUpdatedAt = Nothing,
          billNumber = Nothing,
          feeWithoutDiscount = Nothing,
          schedulerTryCount = 0,
          collectedAt = Nothing
        }

mkInvoiceAgainstDriverFee :: Text -> Text -> UTCTime -> Maybe HighPrecMoney -> INV.InvoicePaymentMode -> DriverFee -> INV.Invoice
mkInvoiceAgainstDriverFee id shortId now maxMandateAmount paymentMode driverFee =
  INV.Invoice
    { id = Id id,
      invoiceShortId = shortId,
      driverFeeId = driverFee.id,
      invoiceStatus = INV.ACTIVE_INVOICE,
      driverId = driverFee.driverId,
      maxMandateAmount,
      paymentMode,
      bankErrorCode = Nothing,
      bankErrorMessage = Nothing,
      bankErrorUpdatedAt = Nothing,
      lastStatusCheckedAt = Nothing,
      updatedAt = now,
      createdAt = now
    }
