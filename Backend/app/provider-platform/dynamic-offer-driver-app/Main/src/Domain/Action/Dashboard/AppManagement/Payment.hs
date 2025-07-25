module Domain.Action.Dashboard.AppManagement.Payment
  ( createPaymentOrder,
    getPaymentOrder,
    getPaymentOrderStatus,
  )
where

import qualified Dashboard.Common
import Domain.Action.UI.Payment
  ( createOrder,
    getOrder,
    getStatus,
  )
import qualified "this" Domain.Action.UI.Payment
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

createPaymentOrder ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.Person ->
  Text ->
  Environment.Flow Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
createPaymentOrder merchantShortId opCity requestorId invoiceId = do
  merchant <- findMerchantByShortId merchantShortId
  opCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  createOrder (ID.cast requestorId, merchant.id, opCityId) $ ID.Id invoiceId

getPaymentOrder ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  ID.Id Dashboard.Common.Person ->
  Environment.Flow DOrder.PaymentOrderAPIEntity
getPaymentOrder merchantShortId opCity orderId requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  getOrder (ID.cast requestorId, merchant.id, merchantOperatingCityId) $ ID.Id orderId

getPaymentOrderStatus ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  ID.Id Dashboard.Common.Person ->
  Environment.Flow Domain.Action.UI.Payment.PaymentStatusResp
getPaymentOrderStatus merchantShortId opCity orderId requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  getStatus (ID.cast requestorId, merchant.id, merchantOperatingCityId) $ ID.Id orderId
