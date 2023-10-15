{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverCoin where

import Data.OpenApi hiding (title)
import Data.Time (UTCTime (utctDay))
import Domain.Types.Coins.CoinHistory
import Domain.Types.Coins.PurchaseHistory
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig ()
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.DriverFee (delCoinAdjustedInSubscriptionByDriverIdKey, getCoinAdjustedInSubscriptionByDriverIdKey)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Coins.PurchaseHistory as PHistory
import Storage.Queries.DriverPlan as DPlan
import Storage.Queries.Person as Person
import Tools.Error

data CoinTransactionHistoryItem = CoinTransactionHistoryItem
  { coins :: Int,
    eventFunction :: DCT.DriverCoinsFunctionType,
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
    createdAt :: UTCTime,
    title :: Text,
    cash :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinsUsageRes = CoinsUsageRes
  { coinBalance :: Int,
    totalCoinConvertedToCash :: HighPrecMoney,
    coinConvertedToCashUsedForLatestDues :: Maybe Int,
    coinConvertedTocashLeft :: HighPrecMoney,
    coinConversionRate :: HighPrecMoney,
    coinUsageHistory :: [CoinUsageHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ConvertCoinToCashReq = ConvertCoinToCashReq
  { cashAmount :: HighPrecMoney,
    coins :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type AccumulationResult = [(Text, Int, CoinStatus)]

getCoinEventSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> UTCTime -> Flow CoinTransactionRes
getCoinEventSummary (driverId, merchantId_, merchantOpCityId) dateInUTC = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId
  let dateInIst = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) dateInUTC
  driver <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
      totalCoinsEarned_ = driver.totalEarnedCoins
      coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinSummary <- CHistory.getCoinEventSummary driverId dateInIst
  let todayCoinSummary_ = sum $ map (.coins) coinSummary
  lastDayHistory <- CHistory.getCoinsEarnedLastDay driverId dateInIst
  let coinsEarnedPreviousDay_ = sum $ map (.coins) lastDayHistory
      coinTransactionHistory = map toTransactionHistoryItem coinSummary
  coinsExpiring <- CHistory.getExpiringCoinsInXDay driverId transporterConfig.coinExpireTime
  let totalExpiringCoins = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) coinsExpiring
      expiringDays_ = fromIntegral (nominalDiffTimeToSeconds transporterConfig.coinExpireTime) `div` 86400
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
        expiringDays = expiringDays_
      }
  where
    toTransactionHistoryItem :: CoinHistory -> CoinTransactionHistoryItem
    toTransactionHistoryItem historyItem =
      CoinTransactionHistoryItem
        { coins = historyItem.coins,
          eventFunction = historyItem.eventFunction,
          createdAt = historyItem.createdAt
        }

getCoinUsageSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow CoinsUsageRes
getCoinUsageSummary (driverId, merchantId_, merchantOpCityId) = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId
  purchaseSummary <- PHistory.getPurchasedHistory driverId
  mbDriverPlan <- DPlan.findByDriverId driverId
  let coinUsageHistory = map toUsageHistoryItem purchaseSummary
  coinAdjustedInSubscriptionKeyExists <- getCoinAdjustedInSubscriptionByDriverIdKey driverId
  coinConvertedToCashUsage <- case coinAdjustedInSubscriptionKeyExists of
    Just cashUsed -> do
      delCoinAdjustedInSubscriptionByDriverIdKey driverId
      pure $ Just cashUsed
    Nothing -> pure Nothing
  let coinConvertedTocashLeft = mbDriverPlan <&> (.coinCovertedToCashLeft)
      totalCoinConvertedToCash = mbDriverPlan <&> (.totalCoinsConvertedCash)
  pure
    CoinsUsageRes
      { coinBalance = coinBalance_,
        totalCoinConvertedToCash = fromMaybe 0.0 totalCoinConvertedToCash,
        coinConversionRate = transporterConfig.coinConversionRate,
        coinUsageHistory = coinUsageHistory,
        coinConvertedTocashLeft = fromMaybe 0.0 coinConvertedTocashLeft,
        coinConvertedToCashUsedForLatestDues = coinConvertedToCashUsage
      }
  where
    toUsageHistoryItem :: PurchaseHistory -> CoinUsageHistoryItem
    toUsageHistoryItem historyItem =
      CoinUsageHistoryItem
        { numCoins = historyItem.numCoins,
          createdAt = historyItem.createdAt,
          cash = historyItem.cash,
          title = historyItem.title
        }

accumulateCoins :: () => Int -> [CoinHistory] -> AccumulationResult
accumulateCoins targetAmount = takeCoinsRequired (targetAmount, []) False
  where
    takeCoinsRequired (_, result) True [] = result
    takeCoinsRequired (_, result) False [] = result
    takeCoinsRequired (toTake, result) _ (coinHis : coinHistories) = do
      let availableCoins = coinHis.coins - coinHis.coinsUsed
          coinsTaken = coinHis.coins
          afterTaking = toTake - availableCoins
      if afterTaking > 0
        then do
          takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken, Used) : result) False coinHistories
        else do
          takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken + afterTaking, Remaining) : result) True []

useCoinsHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> ConvertCoinToCashReq -> Flow APISuccess
useCoinsHandler (driverId, merchantId_, merchantOpCityId) ConvertCoinToCashReq {..} = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  _ <- DPlan.findByDriverId driverId >>= fromMaybeM (InternalError $ "No plan against the driver id" <> driverId.getId <> "Please choose a plan")
  now <- getCurrentTime
  uuid <- generateGUIDText
  coinBalance <- Coins.getCoinsByDriverId driverId
  let currentDate = show $ utctDay now
  if coinBalance >= coins && (fromIntegral coins * transporterConfig.coinConversionRate) == cashAmount
    then do
      let history =
            PurchaseHistory
              { id = Id uuid,
                driverId = driverId.getId,
                merchantId = merchantId_.getId,
                merchantOptCityId = merchantOpCityId.getId,
                numCoins = coins,
                cash = cashAmount,
                createdAt = now,
                updatedAt = now,
                title = "converted from coins"
              }
      void $ PHistory.createPurchaseHistory history
      void $ DPlan.updateCoinFieldsByDriverId driverId cashAmount
      driver <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      void $ Person.updateUsedCoins driverId (coins + driver.usedCoins)
      histories <- CHistory.getDriverCoinInfo driverId
      let result = accumulateCoins coins histories
      mapM_ (\(id, coinValue, status) -> CHistory.updateStatusOfCoins id coinValue status) result
      void $ Hedis.incrby (Coins.mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral (- coins))
    else do
      throwError $ InsufficientCoins driverId.getId
  pure Success
