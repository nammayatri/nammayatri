{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Domain.Action.UI.DriverCoin where

import Data.OpenApi
import Data.Time (UTCTime (utctDay))
import Domain.Types.Coins.CoinHistory
import Domain.Types.Coins.CoinPlan
import Domain.Types.Coins.PurchaseHistory
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.TransporterConfig ()
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (head)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.Types as DCT
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Coins.CoinPlan as CoinPlan
import qualified Storage.Queries.Coins.DriverCoins as DCQ
import Storage.Queries.Coins.PurchaseHistory as PHistory
import Storage.Queries.Person as Person

data CoinTransactionHistoryItem = CoinTransactionHistoryItem
  { coins :: Int,
    fn :: DCT.DriverCoinsFunctionType,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinTransactionRes = CoinTransactionRes
  { coinBalance :: Int,
    coinEarned :: Int,
    coinUsed :: Int,
    coinExpired :: Int,
    todayCoinSummary :: Int,
    coinsEarnedPreviousDay :: Int,
    expiringCoins :: Int,
    expiringDays :: Int,
    coinTransactionHistory :: [CoinTransactionHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinUsageHistoryItem = CoinUsageHistoryItem
  { numCoins :: Int,
    coinPlanName :: Text,
    createdAt :: UTCTime,
    quantity :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinPlanListItem = CoinPlanListItem
  { coinPlanName :: Text,
    coinPlanId :: Text,
    requiredCoins :: Int,
    numOfDays :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinsUsageRes = CoinsUsageRes
  { coinBalance :: Int,
    coinPlanList :: [CoinPlanListItem],
    coinUsageHistory :: [CoinUsageHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PurchasePlanRequest = PurchasePlanRequest
  { coinPlanId :: Text,
    planQuantity :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type AccumulationResult = [(Text, Int, CoinStatus)]

getCoinsByDriveId :: (MonadFlow m, CoreMetrics m, MonadReader r m, CacheFlow m r) => Id SP.Person -> m Int
getCoinsByDriveId driverId = do
  now <- getCurrentTime
  let currentDate = show $ utctDay now
  coinKeyExists <- Coins.getCoinAccumulationByDriverIdKey driverId currentDate
  case coinKeyExists of
    Just coinBalance -> pure coinBalance
    Nothing -> do
      totalCoins <- CHistory.getTotalCoins driverId
      logDebug $ "DB query for CoinBalance of driver" <> show totalCoins
      let total = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) totalCoins
      Coins.setCoinAccumulationByDriverIdKey driverId currentDate total
      pure total

getCoinEventSummary :: (Id SP.Person, Id DM.Merchant) -> UTCTime -> Flow CoinTransactionRes
getCoinEventSummary (driverId, merchantId) dateInUTC = do
  transporterConfig <- TC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ InvalidRequest "Coin Service for this merchant is not available"
  coinBalance_ <- getCoinsByDriveId driverId
  let dateInIst = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) dateInUTC
  driver <- Person.getTotalEarnedAndUsedCoins driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
  let totalCoinsEarned_ = driver.totalEarnedCoins
  let coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinSummary <- CHistory.getCoinEventSummary driverId dateInIst
  let todayCoinSummary_ = sum $ map (\item -> item.coins) coinSummary
  lastDayHistory <- CHistory.getCoinsEarnedLastDay driverId dateInIst
  let coinsEarnedPreviousDay_ = sum $ map (\item -> item.coins) lastDayHistory
  let coinTransactionHistory = map toTransactionHistoryItem coinSummary
  configuration <- DCQ.getCoinInfo merchantId
  let coinConfig = head configuration
  coinsExpiring <- CHistory.getExpiringCoinsInXDay driverId coinConfig.trackExpiry
  let totalExpiringCoins = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) coinsExpiring
  let coinsExpiringInDays = coinConfig.trackExpiry `div` 86400
  pure
    CoinTransactionRes
      { coinBalance = coinBalance_,
        coinEarned = totalCoinsEarned_,
        coinUsed = coinUsed_,
        coinExpired = coinExpired_,
        todayCoinSummary = todayCoinSummary_,
        coinsEarnedPreviousDay = coinsEarnedPreviousDay_,
        coinTransactionHistory = coinTransactionHistory,
        expiringCoins = totalExpiringCoins,
        expiringDays = coinsExpiringInDays
      }
  where
    toTransactionHistoryItem :: CoinHistory -> CoinTransactionHistoryItem
    toTransactionHistoryItem historyItem =
      CoinTransactionHistoryItem
        { coins = historyItem.coins,
          fn = historyItem.fn,
          createdAt = historyItem.createdAt
        }

getCoinUsageSummary :: (Id SP.Person, Id DM.Merchant) -> Flow CoinsUsageRes
getCoinUsageSummary (driverId, merchantId) = do
  transporterConfig <- TC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ InvalidRequest "Coin Service for this merchant is not available"
  coinBalance_ <- getCoinsByDriveId driverId
  purchaseSummary <- PHistory.getPurchasedHistory driverId
  coinPlans <- CoinPlan.getCoinPlans
  coinUsageHistory <- mapM toUsageHistoryItem purchaseSummary
  let coinPlansList = map toCoinPlanListItem coinPlans

  pure
    CoinsUsageRes
      { coinBalance = coinBalance_,
        coinUsageHistory = coinUsageHistory,
        coinPlanList = coinPlansList
      }
  where
    toUsageHistoryItem :: MonadFlow m => PurchaseHistory -> m CoinUsageHistoryItem
    toUsageHistoryItem historyItem = do
      coinPlanName <- CoinPlan.getCoinPlanName (historyItem.coinPlanId)
      pure
        CoinUsageHistoryItem
          { numCoins = historyItem.numCoins,
            coinPlanName = coinPlanName,
            createdAt = historyItem.createdAt,
            quantity = historyItem.quantity
          }

    toCoinPlanListItem :: CoinPlan -> CoinPlanListItem
    toCoinPlanListItem planItem =
      CoinPlanListItem
        { coinPlanName = planItem.coinPlanName,
          requiredCoins = planItem.requiredCoins,
          numOfDays = planItem.numOfDays,
          coinPlanId = planItem.id
        }

accumulateCoins :: () => Int -> [CoinHistory] -> AccumulationResult
accumulateCoins targetAmount = takeCoinsRequired (targetAmount, []) False
  where
    takeCoinsRequired (toTake, result) True [] = result
    takeCoinsRequired (toTake, result) False [] = result
    takeCoinsRequired (toTake, result) _ (coinHis : coinHistories) = do
      let availableCoins = coinHis.coins - coinHis.coinsUsed
          coinsTaken = coinHis.coins
          afterTaking = toTake - availableCoins
      if afterTaking > 0
        then do
          takeCoinsRequired (afterTaking, (coinHis.id, coinsTaken, Used) : result) False coinHistories
        else do
          takeCoinsRequired (afterTaking, (coinHis.id, coinsTaken + afterTaking, Remaining) : result) True []

useCoinsHandler :: (Id SP.Person, Id DM.Merchant) -> PurchasePlanRequest -> Flow APISuccess
useCoinsHandler (driverId, merchantId) PurchasePlanRequest {..} = do
  transporterConfig <- TC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ InvalidRequest "Coin Service for this merchant is not available"
  now <- getCurrentTime
  uuid <- generateGUIDText
  coinPlan <- CoinPlan.getCoinPlanDetails coinPlanId
  coinBalance <- getCoinsByDriveId driverId
  let amount = coinPlan.requiredCoins * planQuantity
  let currentDate = show $ utctDay now
  if coinBalance >= amount
    then do
      let history =
            PurchaseHistory
              { id = uuid,
                driverId = driverId.getId,
                coinPlanId = coinPlanId,
                numCoins = amount,
                quantity = planQuantity,
                quantityLeft = coinPlan.numOfDays * planQuantity,
                createdAt = now
              }
      void $ PHistory.createPurchaseHistory history
      driver <- Person.getTotalEarnedAndUsedCoins driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      void $ Person.updateUsedCoins driverId (amount + driver.usedCoins)
      histories <- CHistory.getDriverCoinInfo driverId
      let result = accumulateCoins amount histories
      mapM_ (\(id, coinValue, status) -> CHistory.updateStatusOfCoins id coinValue status) result
      void $ Hedis.incrby (Coins.mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral (- amount))
    else do
      throwError $ InvalidRequest "Insufficient coin balance of the driver"
  pure Success
