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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption
import Kernel.External.Payment.Interface hiding (createOrder, orderStatus)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

data MandateOrder = MandateOrder
  { maxAmount :: Money,
    _type :: MandateType,
    frequency :: MandateFrequency
  }

createOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  [DriverFee] ->
  Maybe MandateOrder ->
  m Payment.CreateOrderResp
createOrder (driverId, merchantId) driverFees mbMandateOrder = do
  mapM_ (\driverFee -> when (driverFee.status `elem` [CLEARED, EXEMPTED, COLLECTED_CASH]) $ throwError (DriverFeeAlreadySettled driverFee.id.getId)) driverFees
  mapM_ (\driverFee -> when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse driverFee.id.getId)) driverFees
  driver <- runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound $ getId driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let driverEmail = fromMaybe "test@juspay.in" driver.email
      amount = sum $ (\pendingFees -> fromIntegral pendingFees.govtCharges + fromIntegral pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst) <$> driverFees
      dueDriverFeeIds = (.id.getId) <$> driverFees
  orderId <- generateGUID
  orderShortId <- generateShortId
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId,
            orderShortId = orderShortId.getShortId,
            amount = round amount,
            customerId = driver.id.getId,
            customerEmail = driverEmail,
            customerPhone = driverPhone,
            paymentPageClientId = "yatrisathi",
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName,
            createMandate = mbMandateOrder <&> (._type),
            mandateMaxAmount = mbMandateOrder <&> (.maxAmount),
            mandateFrequency = mbMandateOrder <&> (.frequency)
          }
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driver.id
      createOrderCall = Payment.createOrder merchantId -- api call
  DPayment.createOrderService commonMerchantId commonPersonId dueDriverFeeIds createOrderReq createOrderCall
