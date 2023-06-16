{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( DPayment.PaymentStatusResp (..),
    createOrder,
    getStatus,
    juspayWebhookHandler,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPayment
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------

createOrder ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DPayment.PaymentOrder ->
  Flow Payment.CreateOrderResp
createOrder (personId, merchantId) orderId = do
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email")
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  shortId <- generateShortId
  let createOrderReq =
        Payment.CreateOrderReq
          { orderShortId = shortId.getShortId, -- FIXME change type to ShortId -- should be Alphanumeric with character length less than 18.
            amount = 10, -- FIXME
            customerId = person.id.getId,
            customerEmail,
            customerPhone,
            paymentPageClientId = person.id.getId, -- FIXME
            customerFirstName = Just person.firstName,
            customerLastName = person.lastName
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder merchantId -- api call
  DPayment.createOrderService commonMerchantId commonPersonId orderId createOrderReq createOrderCall

-- order status -----------------------------------------------------

getStatus ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  DPayment.orderStatusService commonPersonId orderId orderStatusCall

-- webhook ----------------------------------------------------------

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantShortId authData value = do
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc -> do
      Juspay.orderStatusWebhook psc DPayment.juspayWebhookService authData value
    _ -> throwError $ InternalError "Unknown Service Config"
