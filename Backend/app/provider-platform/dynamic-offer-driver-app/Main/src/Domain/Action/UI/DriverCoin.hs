{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverCoin where

import qualified "this" API.Types.ProviderPlatform.Management.DriverCoins as DCoins hiding (CoinStatus)
import Data.OpenApi hiding (description, title, value)
import qualified Data.Text as Text
import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import Domain.Types.Coins.CoinHistory
import qualified Domain.Types.Coins.CoinsConfig as Fields
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverInformation as Fields
import qualified Domain.Types.DriverStats as Fields
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutConfig as Fields
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import Domain.Types.PurchaseHistory
import qualified Domain.Types.Translations as Fields
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as Fields
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as VecVarient
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Payout.Interface as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.External.Types (Language (..), SchedulerFlow, ServiceFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.DriverCoins.Coins as Coins
import Lib.DriverCoins.Types
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPO
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import SharedLogic.DriverFee (delCoinAdjustedInSubscriptionByDriverIdKey, getCoinAdjustedInSubscriptionByDriverIdKey)
import qualified SharedLogic.Finance.CoinLedger as CoinLedger
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Translations as CQTranslations
import Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import Storage.Queries.DailyStatsExtra as DS
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverPlan as SQPlan
import Storage.Queries.DriverStats as QDS
import Storage.Queries.Person as Person
import Storage.Queries.PurchaseHistory as PHistory
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Encryption
import Tools.Error
import qualified Tools.Payout as Payout

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
    cashWithCurrency :: PriceAPIEntity,
    payoutOrderStatus :: Maybe DS.PayoutStatus,
    vpa :: Maybe Text
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

-- | Both fields are kept so existing apps keep working, but neither is read: the whole balance is used, and the
--   city decides between coin cash and a cash payout.
data ConvertCoinToCashReq = ConvertCoinToCashReq
  { coins :: Int,
    coinRedemptionType :: Maybe CoinRedemptionType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RideStatusPastDaysRes = RideStatusPastDaysRes
  { rideCountPopupValue :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let timeDiffFromUtc = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  let dateInIst = addUTCTime timeDiffFromUtc dateInUTC
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
      totalCoinsEarned_ = driver.totalEarnedCoins
      coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinSummary <- CoinLedger.coinsOfDay driverId dateInIst timeDiffFromUtc
  let todayCoinSummary_ = sum $ map (.coins) coinSummary
  lastDayHistory <- CoinLedger.coinsOfPreviousDay driverId dateInIst timeDiffFromUtc
  let coinsEarnedPreviousDay_ = sum $ map (.coins) lastDayHistory
      coinTransactionHistory = map toTransactionHistoryItem coinSummary
      todayStart = UTCTime (utctDay dateInIst) 0
      coinsExpiredOnThatDay = sumExpiredCoinsOnThatDate coinSummary dateInIst todayStart
  localNow <- addUTCTime timeDiffFromUtc <$> getCurrentTime
  totalExpiringCoins <- CoinLedger.countCoinsExpiringBetween driverId localNow (addUTCTime transporterConfig.coinExpireTime localNow)
  let expiringDays_ = fromIntegral (nominalDiffTimeToSeconds transporterConfig.coinExpireTime) `div` 86400
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

getCoinUsageSummary ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    CoinLedger.CoinFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe CoinRedemptionType ->
  m CoinsUsageRes
getCoinUsageSummary (driverId, merchantId_, merchantOpCityId) mbLimit mbOffset mbCoinRedemptionType = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  purchaseSummary <-
    case mbCoinRedemptionType of
      Just coinRedemptionType -> B.runInReplica $ PHistory.getPurchasedHistoryByCoinRedemptionType driverId mbLimit mbOffset coinRedemptionType
      Nothing -> B.runInReplica $ PHistory.getPurchasedHistory driverId mbLimit mbOffset
  mbDriverStat <- QDS.findById driverId
  coinUsageHistory <- mapM toUsageHistoryItem purchaseSummary
  coinAdjustedInSubscriptionKeyExists <- getCoinAdjustedInSubscriptionByDriverIdKey driverId
  coinConvertedToCashUsage <- case coinAdjustedInSubscriptionKeyExists of
    Just cashUsed -> do
      delCoinAdjustedInSubscriptionByDriverIdKey driverId
      pure $ Just cashUsed
    Nothing -> pure Nothing
  coinCashLeft <- CoinLedger.getCoinCashLeft driverId
  let coinConvertedTocashLeft = Just coinCashLeft
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
    toUsageHistoryItem historyItem = do
      (payoutOrderStatus, vpa) <- case historyItem.payoutOrderIdForDirectPayout of
        Just payoutOrderId -> do
          payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId.getId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId.getId)
          pure $ (Just $ castPayoutOrderStatus payoutOrder.status, payoutOrder.vpa)
        Nothing -> pure (Nothing, Nothing)
      pure $
        CoinUsageHistoryItem
          { numCoins = historyItem.numCoins,
            createdAt = historyItem.createdAt,
            cash = historyItem.cash,
            cashWithCurrency = PriceAPIEntity historyItem.cash historyItem.currency,
            title = historyItem.title,
            payoutOrderStatus = payoutOrderStatus,
            vpa = vpa
          }

-- | One conversion at a time per driver; a second tap while one runs is turned away.
useCoinsHandler ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    CoinLedger.CoinFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ConvertCoinToCashReq ->
  m APISuccess
useCoinsHandler (driverId, merchantId, merchantOpCityId) req =
  Redis.whenWithLockRedisAndReturnValue mkLockKey 30 (handler (driverId, merchantId, merchantOpCityId) req) >>= \case
    Right res -> pure res
    Left () -> throwError (InvalidRequest "Coins conversion already in progress, please wait")
  where
    mkLockKey = "ConvertCoinToCash:DriverId:" <> driverId.getId

-- | The one way a driver uses coins. It takes the whole convertible balance (whole steps of
--   @stepFunctionToConvertCoins@, earliest-expiring coins first) and does what the city says: a cash payout where
--   @enableCoinsToDirectPayout@ is on, otherwise coin cash for the subscription (the nightly fee job uses it).
--   Nothing in the request is read: not the amount, not the redemption type.
handler ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    CoinLedger.CoinFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ConvertCoinToCashReq ->
  m APISuccess
handler (driverId, merchantId_, merchantOpCityId) _req = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  let redemptionType = if fromMaybe False transporterConfig.enableCoinsToDirectPayout then DirectPayout else SubscriptionUse
  when (redemptionType /= DirectPayout) $ do
    mbDriverPlan <- SQPlan.findByDriverIdAndServiceName driverId DPlan.YATRI_SUBSCRIPTION
    when (isNothing mbDriverPlan) $
      throwError $ NoPlanAgaintsDriver driverId.getId
  now <- getCurrentTime
  uuid <- generateGUIDText
  -- the ledger's own figure, not the cached daily balance
  coinBalance <- CoinLedger.getSpendableCoins driverId transporterConfig.timeDiffFromUtc
  let step = max 1 transporterConfig.stepFunctionToConvertCoins
      coinsToConvert = (coinBalance `div` step) * step
      calculatedAmount = fromIntegral coinsToConvert * transporterConfig.coinConversionRate
  when (coinsToConvert < step) $
    throwError $ CoinConversionToCash driverId.getId coinBalance step
  vehCategory <-
    QVeh.findById driverId
      >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
      <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let takeCoins = do
        spent <- CoinLedger.withCoinLock driverId $ CoinLedger.spendCoins driverId transporterConfig.timeDiffFromUtc coinsToConvert "CoinRedemption" uuid
        unless spent $
          throwError $ InsufficientCoins driverId.getId coinsToConvert
      giveCoinsBack =
        CoinLedger.postCoinEvent
          CoinLedger.CoinEvent
            { driverId = driverId,
              merchantId = merchantId_,
              merchantOperatingCityId = merchantOpCityId,
              eventFunction = CoinsRedemptionRefund,
              coins = coinsToConvert,
              expiresAt = Nothing,
              entityId = Just uuid,
              attrs = CoinLedger.CoinAttrs {bulkUploadTitle = Nothing, vehicleCategory = Just vehCategory, serviceTierType = Nothing, earnedAt = Nothing}
            }
  payoutOrderId <-
    case redemptionType of
      DirectPayout -> redeemCoins driverId merchantId_ merchantOpCityId transporterConfig vehCategory driver calculatedAmount takeCoins giveCoinsBack
      SubscriptionUse -> do
        -- take the coins first, then give the coin cash: a failure in between costs the driver coins rather than
        -- handing out cash that no coins paid for
        takeCoins
        CoinLedger.creditCoinCash driverId calculatedAmount "CoinCashRedemption" uuid
        void $ QDS.addToTotalCoinsConvertedCash driverId calculatedAmount
        pure Nothing
  void $
    PHistory.createPurchaseHistory
      PurchaseHistory
        { id = Id uuid,
          driverId = driverId.getId,
          merchantId = merchantId_.getId,
          merchantOptCityId = merchantOpCityId.getId,
          numCoins = coinsToConvert,
          cash = calculatedAmount,
          currency,
          createdAt = now,
          updatedAt = now,
          title = "converted from coins",
          vehicleCategory = Just vehCategory,
          coinRedemptionType = redemptionType,
          payoutOrderIdForDirectPayout = payoutOrderId
        }
  Coins.recordCoinsUsed driverId coinsToConvert transporterConfig.timeDiffFromUtc
  pure Success

-- | Pays the coins out as cash. Everything that can be checked in advance (minimum amount, payout address,
--   payout service) is checked first. @takeCoins@ then runs, just before the provider is called, and
--   @giveCoinsBack@ runs if that call fails, so a provider outage never costs the driver their coins.
redeemCoins ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id SP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DVC.VehicleCategory ->
  SP.Person ->
  HighPrecMoney ->
  m () ->
  m () ->
  m (Maybe (Id DPO.PayoutOrder))
redeemCoins driverId merchantId merchantOpCityId transporterConfig vehCategory driver calculatedAmount takeCoins giveCoinsBack = do
  payoutConfig <- getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, vehicleCategory = Just vehCategory, isPayoutEnabled = Nothing}) Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehCategory) merchantOpCityId.getId)
  case payoutConfig.coinRedemptionMinimumLimit of
    Just coinRedemptionMinimumLimit -> when (calculatedAmount < coinRedemptionMinimumLimit) $ throwError $ InvalidRequest "Calculated amount is less than the coin redemption minimum limit"
    Nothing -> throwError $ InvalidRequest "Coin redemption minimum limit is not set"
  uid <- generateGUID
  phoneNo <- mapM decrypt driver.mobileNumber
  driverInformation <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getCreatePayoutServiceFlow Payout.MerchantServiceUsageConfigOption DEMSC.PayoutService driver.clientSdkVersion merchantOpCityId driver.id
  vpa <- case payoutServiceFlow of
    IPayout.JuspayFlow -> Just <$> (driverInformation.payoutVpa & fromMaybeM (InvalidRequest "Driver has no payout VPA"))
    IPayout.StripeFlow -> pure Nothing
    IPayout.BulkFlow -> pure Nothing -- no VPA on the bulk/HDFC path; Tools.Payout.getCreatePayoutServiceFlow already required a bank account row
  merchantOperatingCity <- CQMOC.findById (cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  let createPayoutOrderReq = DPayment.mkCreatePayoutServiceReq uid calculatedAmount transporterConfig.currency phoneNo driver.email driverId.getId "converted from coins" (Just driver.firstName) vpa payoutConfig.orderType payoutServiceFlow Nothing Nothing
      entityName = DPayment.COINS_REDEMPTION
      createPayoutOrderCall = Payout.createPayoutOrder payoutServiceName merchantOpCityId driver.id mbPersonBankAccount
  takeCoins
  created <- withTryCatch "redeemCoins:createPayout" $ void $ DPayment.createPayoutService (cast merchantId) (Just $ cast merchantOpCityId) (cast driverId) (Just [driverId.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall Nothing afterPayoutOrderCreated
  case created of
    Left err -> do
      logError $ "Coin payout could not be created for driver " <> driverId.getId <> ", giving the coins back: " <> show err
      giveCoinsBack
      throwError $ InternalError "The payout could not be started. Your coins are unchanged, please try again later."
    Right () -> pure ()
  void $ QDS.updateCoinsFieldsForDirectPayout driverId calculatedAmount
  pure $ Just $ Id uid

refundCoins ::
  ( Coins.EventFlow m r,
    PaymentBeamFlow.BeamFlow m r
  ) =>
  Id SP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m ()
refundCoins driverId merchantId merchantOperatingCityId payoutOrderId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
  mbVehicle <- QVeh.findById driverId
  let vehCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
      numCoins = round (payoutOrder.amount.amount / transporterConfig.coinConversionRate)
  void $ Coins.updateEventAndGetCoinsvalue driverId merchantId merchantOperatingCityId CoinsRedemptionRefund Nothing numCoins (Just payoutOrder.id.getId) vehCategory Nothing
  Coins.updateDriverCoins driverId numCoins transporterConfig.timeDiffFromUtc
  QDS.updateCoinsFieldsForDirectPayout driverId (negate payoutOrder.amount.amount)

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DS.PayoutStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DS.Success
    Payout.ERROR -> DS.Failed
    Payout.FAILURE -> DS.Failed
    Payout.FULFILLMENTS_FAILURE -> DS.Failed
    Payout.CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DS.ManualReview
    _ -> DS.Processing

getRideStatusPastDays :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow RideStatusPastDaysRes
getRideStatusPastDays (driverId, merchantId_, merchantOpCityId) = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $ throwError $ CoinServiceUnavailable merchantId.getId
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehCategory <-
    QVeh.findById driverId
      >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
      <&> (\vehicle -> VecVarient.castVehicleVariantToVehicleCategory vehicle.variant)
  let language = fromMaybe ENGLISH (driver.language)
  activeConfigs <- getConfig (CoinsConfigDimensions {merchantOptCityId = merchantOpCityId.getId, merchantId = Just merchantId.getId, active = Just True, vehicleCategory = Just vehCategory, eventFunction = Nothing, serviceTierType = Nothing, eventName = Nothing, tripCategoryType = Nothing, configId = Nothing}) (Just (SQCC.getActiveCoinConfigs merchantId merchantOpCityId vehCategory))
  coinConfigRes <-
    mapM
      ( \activeConfg -> do
          let id = Text.strip (activeConfg.eventName <> "_" <> (show activeConfg.eventFunction))
          translation <- CQTranslations.findByOpsKey (Just merchantOpCityId) language id >>= fromMaybeM (CoinInfoTranslationNotFound id (show language))
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
