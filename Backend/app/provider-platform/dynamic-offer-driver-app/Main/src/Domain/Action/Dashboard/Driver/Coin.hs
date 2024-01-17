{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Domain.Action.Dashboard.Driver.Coin where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as Common
import Data.Time (UTCTime (UTCTime, utctDay))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import Domain.Types.Coins.PurchaseHistory as PurchaseHistory
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Coins.PurchaseHistory as PHistory
import Storage.Queries.Person as Person
import Tools.Error

bulkUploadCoinsHandler :: ShortId DM.Merchant -> Context.City -> Common.BulkUploadCoinsReq -> Flow APISuccess
bulkUploadCoinsHandler merchantShortId opCity Common.BulkUploadCoinsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchant.id.getId
  mapM_ (\Common.DriverIdListWithCoins {..} -> bulkUpdateByDriverId merchant.id merchantOpCityId (Id driverId :: Id SP.Person) DCT.BulkUploadFunction coins bulkUploadTitle expirationTime transporterConfig) driverIdListWithCoins
  pure Success

bulkUpdateByDriverId ::
  ( MonadFlow m,
    MonadReader r m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SP.Person ->
  DCT.DriverCoinsFunctionType ->
  Int ->
  Common.Translations ->
  Maybe Int ->
  TransporterConfig ->
  m ()
bulkUpdateByDriverId merchantId merchantOpCityId driverId eventFunction coinsValue bulkUploadTitle mbexpirationTime transporterConfig = do
  if coinsValue < 0
    then do
      logError $ "Coins value cannot be negative in bulk upload for driverId: " <> show driverId <> " coin value: " <> show coinsValue
    else do
      Coins.updateDriverCoins driverId coinsValue transporterConfig.timeDiffFromUtc
      now <- getCurrentTime
      uuid <- generateGUIDText
      let expiryTime = fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
          status_ = if coinsValue > 0 then DTCC.Remaining else DTCC.Used
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
                bulkUploadTitle = Just bulkUploadTitle
              }
      CHistory.updateCoinEvent driverCoinEvent
      Coins.sendCoinsNotification merchantOpCityId driverId coinsValue
      pure ()

coinHistoryHandler :: ShortId DM.Merchant -> Context.City -> Id SP.Person -> Maybe Integer -> Maybe Integer -> Flow Common.CoinHistoryRes
coinHistoryHandler merchantShortId opCity driverId mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
    toEarnHistoryItem DTCC.CoinHistory {..} =
      Common.CoinEarnHistoryItem
        { coins = coins,
          status = castStatusForCoins status,
          eventFunction = castEventFunctionForCoins eventFunction,
          createdAt = createdAt,
          expirationAt = expirationAt,
          coinsUsed = coinsUsed,
          bulkUploadTitle = bulkUploadTitle
        }

    toBurnHistoryItem :: PurchaseHistory -> Common.CoinBurnHistoryItem
    toBurnHistoryItem PurchaseHistory {..} =
      Common.CoinBurnHistoryItem
        { numCoins = numCoins,
          cash = cash,
          title = title,
          createdAt = createdAt,
          updatedAt = updatedAt
        }

castStatusForCoins :: DTCC.CoinStatus -> Common.CoinStatus
castStatusForCoins s = case s of
  DTCC.Used -> Common.Used
  DTCC.Remaining -> Common.Remaining

castEventFunctionForCoins :: DCT.DriverCoinsFunctionType -> Common.DriverCoinsFunctionType
castEventFunctionForCoins e = case e of
  DCT.BulkUploadFunction -> Common.BulkUploadFunction
  DCT.OneOrTwoStarRating -> Common.OneOrTwoStarRating
  DCT.RideCompleted -> Common.RideCompleted
  DCT.FiveStarRating -> Common.FiveStarRating
  DCT.BookingCancellation -> Common.BookingCancellation
  DCT.CustomerReferral -> Common.CustomerReferral
  DCT.DriverReferral -> Common.DriverReferral
  DCT.EightPlusRidesInOneDay -> Common.EightPlusRidesInOneDay
  DCT.PurpleRideCompleted -> Common.PurpleRideCompleted
  DCT.LeaderBoardTopFiveHundred -> Common.LeaderBoardTopFiveHundred
  DCT.TrainingCompleted -> Common.TrainingCompleted
