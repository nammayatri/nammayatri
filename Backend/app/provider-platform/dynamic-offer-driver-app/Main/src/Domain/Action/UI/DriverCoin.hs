{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverCoin where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverCoins as DCoins hiding (CoinStatus)
import Data.OpenApi hiding (description, title, value)
import qualified Data.Text as Text
import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import Domain.Types.Coins.CoinHistory
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import Domain.Types.PurchaseHistory
import Domain.Types.TransporterConfig ()
import qualified Domain.Types.VehicleVariant as VecVarient
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Types (Language (..))
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import Lib.DriverCoins.Types
import SharedLogic.DriverFee (delCoinAdjustedInSubscriptionByDriverIdKey, getCoinAdjustedInSubscriptionByDriverIdKey)
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Coins.CoinsConfig as SQCC
import Storage.Queries.DailyStatsExtra as DS
import Storage.Queries.DriverPlan as SQPlan
import Storage.Queries.DriverStats as QDS
import Storage.Queries.Person as Person
import Storage.Queries.PurchaseHistory as PHistory
import Storage.Queries.TranslationsExtra as SQT
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

data CoinTransactionHistoryItem = CoinTransactionHistoryItem
  { coins :: Int,
    eventFunction :: DriverCoinsFunctionType,
    bulkUploadTitle :: Maybe DCoins.Translations,
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
    coinsExpiredOnThatDay :: Int,
    coinTransactionHistory :: [CoinTransactionHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinUsageHistoryItem = CoinUsageHistoryItem
  { numCoins :: Int,
    createdAt :: UTCTime,
    title :: Text,
    cash :: HighPrecMoney,
    cashWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinsUsageRes = CoinsUsageRes
  { coinBalance :: Int,
    totalCoinConvertedToCash :: HighPrecMoney,
    totalCoinConvertedToCashWithCurrency :: PriceAPIEntity,
    coinConvertedToCashUsedForLatestDues :: Maybe Int,
    coinConvertedTocashLeft :: HighPrecMoney,
    coinConvertedToCashLeftWithCurrency :: PriceAPIEntity,
    coinConversionRate :: HighPrecMoney,
    coinConversionRateWithCurrency :: PriceAPIEntity,
    coinUsageHistory :: [CoinUsageHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RideStatusPastDaysRes = RideStatusPastDaysRes
  { rideCountPopupValue :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ConvertCoinToCashReq = ConvertCoinToCashReq
  { coins :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type AccumulationResult = [(Text, Int, CoinStatus)]

newtype CoinInfoRes = CoinInfoRes [CoinInfo]
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinInfo = CoinInfo
  { coins :: Int,
    key :: Text,
    title :: Text,
    description :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getCoinEventSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> UTCTime -> Flow CoinTransactionRes
getCoinEventSummary (driverId, merchantId_, merchantOpCityId) dateInUTC = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let timeDiffFromUtc = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  let dateInIst = addUTCTime timeDiffFromUtc dateInUTC
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
      totalCoinsEarned_ = driver.totalEarnedCoins
      coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinSummary <- B.runInReplica $ CHistory.getCoinEventSummary driverId dateInIst timeDiffFromUtc
  let todayCoinSummary_ = sum $ map (.coins) coinSummary
  lastDayHistory <- B.runInReplica $ CHistory.getCoinsEarnedLastDay driverId dateInIst timeDiffFromUtc
  let coinsEarnedPreviousDay_ = sum $ map (.coins) lastDayHistory
      coinTransactionHistory = map toTransactionHistoryItem coinSummary
      todayStart = UTCTime (utctDay dateInIst) 0
      coinsExpiredOnThatDay = sumExpiredCoinsOnThatDate coinSummary dateInIst todayStart
  coinsExpiring <- B.runInReplica $ CHistory.getExpiringCoinsInXDay driverId transporterConfig.coinExpireTime timeDiffFromUtc
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
        expiringDays = expiringDays_,
        coinsExpiredOnThatDay = coinsExpiredOnThatDay
      }
  where
    toTransactionHistoryItem :: CoinHistory -> CoinTransactionHistoryItem
    toTransactionHistoryItem historyItem =
      CoinTransactionHistoryItem
        { coins = historyItem.coins,
          eventFunction = historyItem.eventFunction,
          createdAt = historyItem.createdAt,
          bulkUploadTitle = historyItem.bulkUploadTitle
        }

sumExpiredCoinsOnThatDate :: [CoinHistory] -> UTCTime -> UTCTime -> Int
sumExpiredCoinsOnThatDate coinHistories time todayStart = do
  sum $ map (.coins) $ filter isExpirationOnThatDate coinHistories
  where
    isExpirationOnThatDate :: CoinHistory -> Bool
    isExpirationOnThatDate historyItem =
      case expirationAt historyItem of
        Just expiration -> expiration >= todayStart && expiration < time
        Nothing -> False

getCoinUsageSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Integer -> Maybe Integer -> Flow CoinsUsageRes
getCoinUsageSummary (driverId, merchantId_, merchantOpCityId) mbLimit mbOffset = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  purchaseSummary <- B.runInReplica $ PHistory.getPurchasedHistory driverId mbLimit mbOffset
  mbDriverStat <- QDS.findById driverId
  let coinUsageHistory = map toUsageHistoryItem purchaseSummary
  coinAdjustedInSubscriptionKeyExists <- getCoinAdjustedInSubscriptionByDriverIdKey driverId
  coinConvertedToCashUsage <- case coinAdjustedInSubscriptionKeyExists of
    Just cashUsed -> do
      delCoinAdjustedInSubscriptionByDriverIdKey driverId
      pure $ Just cashUsed
    Nothing -> pure Nothing
  let coinConvertedTocashLeft = mbDriverStat <&> (.coinCovertedToCashLeft)
      totalCoinConvertedToCash = mbDriverStat <&> (.totalCoinsConvertedCash)
  let currency = fromMaybe transporterConfig.currency (mbDriverStat <&> (.currency))
  pure
    CoinsUsageRes
      { coinBalance = coinBalance_,
        totalCoinConvertedToCash = fromMaybe 0.0 totalCoinConvertedToCash,
        totalCoinConvertedToCashWithCurrency = PriceAPIEntity (fromMaybe 0.0 totalCoinConvertedToCash) currency,
        coinConversionRate = transporterConfig.coinConversionRate,
        coinConversionRateWithCurrency = PriceAPIEntity transporterConfig.coinConversionRate currency,
        coinUsageHistory = coinUsageHistory,
        coinConvertedTocashLeft = fromMaybe 0.0 coinConvertedTocashLeft,
        coinConvertedToCashLeftWithCurrency = PriceAPIEntity (fromMaybe 0.0 coinConvertedTocashLeft) currency,
        coinConvertedToCashUsedForLatestDues = coinConvertedToCashUsage
      }
  where
    toUsageHistoryItem :: PurchaseHistory -> CoinUsageHistoryItem
    toUsageHistoryItem historyItem =
      CoinUsageHistoryItem
        { numCoins = historyItem.numCoins,
          createdAt = historyItem.createdAt,
          cash = historyItem.cash,
          cashWithCurrency = PriceAPIEntity historyItem.cash historyItem.currency,
          title = historyItem.title
        }

accumulateCoins :: Int -> [CoinHistory] -> AccumulationResult
accumulateCoins targetAmount = takeCoinsRequired (targetAmount, []) False
  where
    takeCoinsRequired :: (Int, AccumulationResult) -> Bool -> [CoinHistory] -> AccumulationResult
    takeCoinsRequired (_, result) True [] = result
    takeCoinsRequired (_, result) False [] = result
    takeCoinsRequired (toTake, result) _ (coinHis : coinHistories) = do
      let availableCoins = coinHis.coins - coinHis.coinsUsed
          coinsTaken = coinHis.coins
          afterTaking = toTake - availableCoins
          coinStatus = bool Remaining Used (afterTaking == 0)
      if afterTaking > 0
        then takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken, Used) : result) False coinHistories
        else takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken + afterTaking, coinStatus) : result) True []

useCoinsHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> ConvertCoinToCashReq -> Flow APISuccess
useCoinsHandler (driverId, merchantId_, merchantOpCityId) ConvertCoinToCashReq {..} = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  whenM (noDriverPlan driverId) $
    throwError $ NoPlanAgaintsDriver driverId.getId
  now <- getCurrentTime
  uuid <- generateGUIDText
  coinBalance <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let stepFunctionToConvertCoins = transporterConfig.stepFunctionToConvertCoins
      timeDiffFromUtc = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  when (coins < stepFunctionToConvertCoins) $
    throwError $ CoinConversionToCash driverId.getId coins stepFunctionToConvertCoins
  when (coins `mod` stepFunctionToConvertCoins /= 0) $
    throwError $ CoinUsedForConverting driverId.getId coins stepFunctionToConvertCoins
  let istTime = addUTCTime timeDiffFromUtc now
      currentDate = show $ utctDay istTime
      calculatedAmount = fromIntegral coins * transporterConfig.coinConversionRate
  vehCategory <-
    QVeh.findById driverId
      >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
      <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
  if coinBalance >= coins
    then do
      let history =
            PurchaseHistory
              { id = Id uuid,
                driverId = driverId.getId,
                merchantId = merchantId_.getId,
                merchantOptCityId = merchantOpCityId.getId,
                numCoins = coins,
                cash = calculatedAmount,
                currency,
                createdAt = now,
                updatedAt = now,
                title = "converted from coins",
                vehicleCategory = Just vehCategory
              }

      void $ PHistory.createPurchaseHistory history
      void $ QDS.updateCoinFieldsByDriverId driverId calculatedAmount
      driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      -- Deduct Existing Coin Amount from the drivers coin history
      histories <- CHistory.getDriverCoinInfo driverId timeDiffFromUtc
      let result = accumulateCoins coins histories
      mapM_ (\(id, coinValue, status) -> CHistory.updateStatusOfCoins id coinValue status) result
      void $ Person.updateUsedCoins (coins + driver.usedCoins) driverId
      Coins.safeIncrBy (Coins.mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral (- coins)) driverId transporterConfig.timeDiffFromUtc
    else do
      throwError $ InsufficientCoins driverId.getId coins
  pure Success
  where
    noDriverPlan :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SP.Person -> m Bool
    noDriverPlan driverId_ = isNothing <$> SQPlan.findByDriverIdAndServiceName driverId_ DPlan.YATRI_SUBSCRIPTION

getRideStatusPastDays :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow RideStatusPastDaysRes
getRideStatusPastDays (driverId, merchantId_, merchantOpCityId) = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  localTimeToday <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let localDateToday = utctDay localTimeToday
      pastDays = toInteger $ transporterConfig.pastDaysRideCounter
      daysAgo = addDays (- pastDays) localDateToday
  dailyRideSummary <- DS.findAllInRangeByDriverId driverId daysAgo localDateToday
  let ridesExceedThreshold = case dailyRideSummary of
        [] -> False
        _ -> length dailyRideSummary >= transporterConfig.pastDaysRideCounter && all (\res -> res.numRides > transporterConfig.pastDaysRideCounter) dailyRideSummary
  pure $ RideStatusPastDaysRes {rideCountPopupValue = ridesExceedThreshold}

getCoinsInfo :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow CoinInfoRes
getCoinsInfo (driverId, merchantId, merchantOpCityId) = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $ throwError $ CoinServiceUnavailable merchantId.getId
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehCategory <-
    QVeh.findById driverId
      >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
      <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
  let language = fromMaybe ENGLISH (driver.language)
  activeConfigs <- SQCC.getActiveCoinConfigs merchantId merchantOpCityId vehCategory
  coinConfigRes <-
    mapM
      ( \activeConfg -> do
          let id = Text.strip (activeConfg.eventName <> "_" <> (show activeConfg.eventFunction))
          translation <- SQT.findByErrorAndLanguage id language >>= fromMaybeM (CoinInfoTranslationNotFound id (show language))
          case Text.splitOn " | " translation.message of
            [title, description] ->
              pure $
                CoinInfo
                  { coins = activeConfg.coins,
                    key = id,
                    title = title,
                    description = description
                  }
            [title] ->
              pure $
                CoinInfo
                  { coins = activeConfg.coins,
                    key = id,
                    title = title,
                    description = ""
                  }
            _ -> throwError $ CoinInfoTranslationNotFound id (show language)
      )
      activeConfigs
  pure $ CoinInfoRes coinConfigRes
