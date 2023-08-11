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
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

data MandateOrder = MandateOrder
  { maxAmount :: HighPrecMoney,
    _type :: MandateType,
    frequency :: MandateFrequency
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
  [DriverFee] ->
  Maybe MandateOrder ->
  m (CreateOrderResp, Id DOrder.PaymentOrder)
createOrder (driverId, merchantId) driverFees mbMandateOrder = do
  mapM_ (\driverFee -> when (driverFee.status `elem` [CLEARED, EXEMPTED, COLLECTED_CASH]) $ throwError (DriverFeeAlreadySettled driverFee.id.getId)) driverFees
  mapM_ (\driverFee -> when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse driverFee.id.getId)) driverFees
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound $ getId driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  invoiceId <- generateGUID
  invoiceShortId <- generateShortId
  now <- getCurrentTime
  let driverEmail = fromMaybe "test@juspay.in" driver.email
      amount = sum $ (\pendingFees -> fromIntegral pendingFees.govtCharges + fromIntegral pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst) <$> driverFees
      invoices = mkInvoiceAgainstDriverFee invoiceId invoiceShortId.getShortId now <$> driverFees
  QIN.createMany invoices
  let createOrderReq =
        CreateOrderReq
          { orderId = invoiceId,
            orderShortId = invoiceShortId.getShortId,
            amount = amount,
            customerId = driver.id.getId,
            customerEmail = driverEmail,
            customerPhone = driverPhone,
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName,
            createMandate = mbMandateOrder <&> (._type),
            mandateMaxAmount = mbMandateOrder <&> (.maxAmount),
            mandateFrequency = mbMandateOrder <&> (.frequency)
          }
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driver.id
      createOrderCall = Payment.createOrder merchantId -- api call
  createOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  return (createOrderRes, Id invoiceId)

mkInvoiceAgainstDriverFee :: Text -> Text -> UTCTime -> DriverFee -> INV.Invoice
mkInvoiceAgainstDriverFee id shortId now driverFee =
  INV.Invoice
    { id = id,
      invoiceShortId = shortId,
      driverFeeId = driverFee.id,
      updatedAt = now,
      createdAt = now
    }
