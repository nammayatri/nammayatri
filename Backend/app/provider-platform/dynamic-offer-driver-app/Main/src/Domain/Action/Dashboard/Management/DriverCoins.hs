{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.DriverCoins
  ( postDriverCoinsBulkUploadCoins,
    postDriverCoinsBulkUploadCoinsV2,
    getDriverCoinsCoinHistory,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverCoins as Common
import Data.Time (UTCTime (UTCTime, utctDay))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.PurchaseHistory as PurchaseHistory
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleVariant as VecVarient
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import Lib.DriverCoins.Types
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Person as Person
import Storage.Queries.PurchaseHistory as PHistory
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

postDriverCoinsBulkUploadCoins :: ShortId DM.Merchant -> Context.City -> Common.BulkUploadCoinsReq -> Flow APISuccess
postDriverCoinsBulkUploadCoins merchantShortId opCity Common.BulkUploadCoinsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchant.id.getId
  mapM_ (\Common.DriverIdListWithCoins {..} -> bulkUpdateByDriverId merchant.id merchantOpCityId (Id driverId :: Id SP.Person) BulkUploadFunction coins bulkUploadTitle expirationTime transporterConfig Nothing) driverIdListWithCoins
  pure Success

bulkUpdateByDriverId ::
  ( MonadFlow m,
    MonadReader r m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SP.Person ->
  DriverCoinsFunctionType ->
  Int ->
  Common.Translations ->
  Maybe Int ->
  TransporterConfig ->
  Maybe Text ->
  m ()
bulkUpdateByDriverId merchantId merchantOpCityId driverId eventFunction coinsValue bulkUploadTitle mbexpirationTime transporterConfig entityId = do
  if coinsValue < 0
    then do
      logError $ "Coins value cannot be negative in bulk upload for driverId: " <> show driverId <> " coin value: " <> show coinsValue
    else do
      Coins.updateDriverCoins driverId coinsValue transporterConfig.timeDiffFromUtc
      now <- getCurrentTime
      uuid <- generateGUIDText
      let expiryTime = fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
          status_ = if coinsValue > 0 then Remaining else Used
      vehCategory <-
        QVeh.findById driverId
          >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
          <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
      let driverCoinEvent =
            DTCC.CoinHistory
              { id = Id uuid,
                driverId = driverId.getId,
                merchantId = merchantId.getId,
                merchantOptCityId = merchantOpCityId.getId,
                eventFunction = eventFunction,
                coins = coinsValue,
                status = status_,
                createdAt = now,
                updatedAt = now,
                expirationAt = expiryTime,
                coinsUsed = 0,
                bulkUploadTitle = Just bulkUploadTitle,
                entityId = entityId,
                vehicleCategory = Just vehCategory
              }
      CHistory.updateCoinEvent driverCoinEvent
      Coins.sendCoinsNotification merchantOpCityId driverId coinsValue eventFunction
      pure ()

postDriverCoinsBulkUploadCoinsV2 :: ShortId DM.Merchant -> Context.City -> Common.BulkUploadCoinsReqV2 -> Flow APISuccess
postDriverCoinsBulkUploadCoinsV2 merchantShortId opCity Common.BulkUploadCoinsReqV2 {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchant.id.getId
  SMerchant.checkCurrencies merchantOpCity.currency $ driverIdListWithCoins <&> (.amountWithCurrency)
  mapM_ (\Common.DriverIdListWithAmount {..} -> bulkUpdateByDriverIdV2 merchant.id merchantOpCity.id (Id driverId :: Id SP.Person) eventFunction (fromMaybe amount $ amountWithCurrency <&> (.amount)) bulkUploadTitle expirationTime transporterConfig Nothing) driverIdListWithCoins
  pure Success

bulkUpdateByDriverIdV2 ::
  ( MonadFlow m,
    MonadReader r m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SP.Person ->
  DriverCoinsFunctionType ->
  HighPrecMoney ->
  Common.Translations ->
  Maybe Int ->
  TransporterConfig ->
  Maybe Text ->
  m ()
bulkUpdateByDriverIdV2 merchantId merchantOpCityId driverId eventFunction amount bulkUploadTitle mbexpirationTime transporterConfig entityId = do
  case eventFunction of
    BulkUploadFunctionV2 _ -> do
      let coinsValue_ = amount.getHighPrecMoney / transporterConfig.coinConversionRate.getHighPrecMoney
          coinsValue = round coinsValue_
      Coins.updateDriverCoins driverId coinsValue transporterConfig.timeDiffFromUtc
      now <- getCurrentTime
      uuid <- generateGUIDText
      let expiryTime = fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
          status_ = if coinsValue > 0 then Remaining else Used
      vehCategory <-
        QVeh.findById driverId
          >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
          <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
      let driverCoinEvent =
            DTCC.CoinHistory
              { id = Id uuid,
                driverId = driverId.getId,
                merchantId = merchantId.getId,
                merchantOptCityId = merchantOpCityId.getId,
                eventFunction = eventFunction,
                coins = coinsValue,
                status = status_,
                createdAt = now,
                updatedAt = now,
                expirationAt = expiryTime,
                coinsUsed = 0,
                bulkUploadTitle = Just bulkUploadTitle,
                entityId = entityId,
                vehicleCategory = Just vehCategory
              }
      CHistory.updateCoinEvent driverCoinEvent
      Coins.sendCoinsNotificationV2 merchantOpCityId driverId amount coinsValue eventFunction
    otherEventFunction -> throwError $ NonBulkUploadCoinFunction $ show otherEventFunction

getDriverCoinsCoinHistory :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Integer -> Maybe Integer -> Flow Common.CoinHistoryRes
getDriverCoinsCoinHistory merchantShortId opCity reqDriverId mbLimit mbOffset = do
  let driverId = cast @Common.Driver @SP.Driver reqDriverId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchant.id.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
      totalCoinsEarned_ = driver.totalEarnedCoins
      coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinEarnSummary <- B.runInReplica $ CHistory.totalCoinEarnHistory driverId mbLimit mbOffset
  coinBurnSummary <- B.runInReplica $ PHistory.getPurchasedHistory driverId mbLimit mbOffset
  let coinEarnHistory = map toEarnHistoryItem coinEarnSummary
      coinBurnHistory = map toBurnHistoryItem coinBurnSummary
  pure
    Common.CoinHistoryRes
      { coinBalance = coinBalance_,
        coinEarned = totalCoinsEarned_,
        coinUsed = coinUsed_,
        coinExpired = coinExpired_,
        coinEarnHistory = coinEarnHistory,
        coinBurnHistory = coinBurnHistory
      }
  where
    toEarnHistoryItem :: DTCC.CoinHistory -> Common.CoinEarnHistoryItem
    toEarnHistoryItem DTCC.CoinHistory {driverId = _driverId, ..} =
      Common.CoinEarnHistoryItem
        { coins = coins,
          status = status,
          eventFunction = eventFunction,
          createdAt = createdAt,
          expirationAt = expirationAt,
          coinsUsed = coinsUsed,
          bulkUploadTitle = bulkUploadTitle
        }

    toBurnHistoryItem :: PurchaseHistory -> Common.CoinBurnHistoryItem
    toBurnHistoryItem PurchaseHistory {driverId = _driverId, ..} =
      Common.CoinBurnHistoryItem
        { numCoins = numCoins,
          cash = cash,
          cashWithCurrency = PriceAPIEntity cash currency,
          title = title,
          createdAt = createdAt,
          updatedAt = updatedAt
        }
