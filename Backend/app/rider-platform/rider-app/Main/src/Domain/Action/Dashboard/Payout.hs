{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Payout
  ( getPayoutPayoutOrder,
  )
where

import Data.Time (minutesToTimeZone, utcToLocalTime)
import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Minutes)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getPayoutPayoutOrder ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity payoutOrderId = do
  (merchant, _, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
  unless (payoutOrder.merchantId == merchant.id.getId) $
    throwError $ PayoutOrderNotFound payoutOrderId
  unless (payoutOrder.city == show opCity) $
    throwError $ PayoutOrderNotFound payoutOrderId
  refreshedOrder <- UIPayout.refreshPayoutOrderWithSettlement payoutOrder
  buildPayoutOrderResp timeZoneDiff refreshedOrder

resolveMerchantOpCityAndTz ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow (DM.Merchant, DMOC.MerchantOperatingCity, Minutes)
resolveMerchantOpCityAndTz merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  riderConfig <-
    getConfig
      (RiderConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId})
      (Just (CQRC.findByMerchantOperatingCityId merchantOpCity.id))
      >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOpCity.id.getId)
  pure (merchant, merchantOpCity, secondsToMinutes riderConfig.timeDiffFromUtc)

buildPayoutOrderResp ::
  Minutes ->
  PayoutOrder.PayoutOrder ->
  Flow PayoutTypes.PayoutOrderResp
buildPayoutOrderResp timeZoneDiff payoutOrder = do
  let timeZone = minutesToTimeZone timeZoneDiff.getMinutes
  person <- QPerson.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
  phoneNo <- decrypt payoutOrder.mobileNo
  pure
    PayoutTypes.PayoutOrderResp
      { payoutOrderId = payoutOrder.orderId,
        payoutOrderDbId = payoutOrder.id,
        driverId = payoutOrder.customerId,
        driverName = fromMaybe "" person.firstName,
        driverPhoneNo = phoneNo,
        amount = payoutOrder.amount.amount,
        transferAmount = payoutOrder.transferAmount,
        status = show payoutOrder.status,
        entityName = payoutOrder.entityName,
        entityIds = payoutOrder.entityIds,
        responseMessage = payoutOrder.responseMessage,
        responseCode = payoutOrder.responseCode,
        retriedOrderId = payoutOrder.retriedOrderId,
        vpa = payoutOrder.vpa,
        payoutTime = utcToLocalTime timeZone payoutOrder.createdAt,
        createdAt = payoutOrder.createdAt,
        updatedAt = payoutOrder.updatedAt
      }
