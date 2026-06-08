{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.CoinLedger
  ( EventFlow,
    awardCoinsFromJsonLogic,
    awardCashFromJsonLogic,
    updateEventAndGetCoinsvalue,
    updateEventAndGetMonetaryRewardCredit,
  )
where

import Data.Time (UTCTime (UTCTime, utctDay))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.TransporterConfig
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.DriverCoins.CoinBalance as CoinBalance
import qualified Lib.DriverCoins.CoinNotifications as CoinNotifications
import Lib.DriverCoins.Types
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import qualified SharedLogic.Finance.Wallet as SLFW
import qualified SharedLogic.Rewards.Types as RewardTypes
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.Coins.CoinHistory as QCoinHistory
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m, ClickhouseFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r)

awardCoinsFromJsonLogic ::
  (EventFlow m r, Hedis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  Maybe DTC.ServiceTierType ->
  DCT.DriverCoinsFunctionType ->
  Int ->
  Maybe Int ->
  Maybe Text ->
  m ()
awardCoinsFromJsonLogic driverId merchantId merchantOpCityId vehCategory mbServiceTierType eventFunction coins mbExpirationHours entityId = do
  mbExisting <- QCoinHistory.getCoinsByEventFunction driverId eventFunction entityId
  when (isJust mbExisting) $
    logInfo $ "Skipping duplicate AWARD_COINS for driver " <> driverId.getId <> " eventFunction=" <> show eventFunction
  unless (isJust mbExisting) $
    void $
      updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbExpirationHours coins entityId vehCategory mbServiceTierType

awardCashFromJsonLogic ::
  EventFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  RewardTypes.RewardDispatchContext ->
  DCT.DriverCoinsFunctionType ->
  HighPrecMoney ->
  Text ->
  Maybe (Id DP.Person) ->
  m ()
awardCashFromJsonLogic driverId merchantId merchantOpCityId rewardCtx eventFunction amount referenceId mbFleetOwnerId =
  void $
    updateEventAndGetMonetaryRewardCredit driverId merchantId merchantOpCityId eventFunction amount rewardCtx.transporterConfig (Just referenceId) mbFleetOwnerId

updateEventAndGetCoinsvalue :: (EventFlow m r, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currentBalance <- CoinBalance.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let expiryTime =
        if currentBalance < 0
          then Nothing
          else fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
      status_ = if numCoins > 0 then Remaining else Used
  let driverCoinEvent =
        DTCC.CoinHistory
          { id = Id uuid,
            driverId = driverId.getId,
            merchantId = merchantId.getId,
            merchantOptCityId = merchantOpCityId.getId,
            eventFunction = eventFunction,
            coins = numCoins,
            status = status_,
            createdAt = now,
            updatedAt = now,
            expirationAt = expiryTime,
            coinsUsed = 0,
            bulkUploadTitle = Nothing,
            entityId = entityId,
            vehicleCategory = Just vehCategory,
            serviceTierType = mbServiceTierType
          }
  CHistory.updateCoinEvent driverCoinEvent

  case eventFunction of
    DCT.BookingCancellationPenalisaton -> do
      CoinNotifications.sendCoinsNotification merchantOpCityId driverId numCoins eventFunction
    _ -> do
      when (numCoins > 0) $ do
        case eventFunction of
          DCT.MetroRideCompleted _ _ -> do
            logDebug "metro notification case for coins"
            CoinNotifications.sendCoinsNotificationV3 merchantOpCityId driverId numCoins eventFunction
          _ -> CoinNotifications.sendCoinsNotification merchantOpCityId driverId numCoins eventFunction
  pure numCoins

updateEventAndGetMonetaryRewardCredit :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> Maybe (Id DP.Person) -> m Bool
updateEventAndGetMonetaryRewardCredit driverId merchantId merchantOpCityId eventFunction amount transporterConfig entityId mbFleetOwnerId = do
  let referenceId = fromMaybe "wallet_incentive" entityId
  logDebug $
    "Wallet Incentive: crediting driver wallet"
      <> " | driverId="
      <> driverId.getId
      <> " | eventFunction="
      <> show eventFunction
      <> " | amount="
      <> show amount
      <> " | currency="
      <> show transporterConfig.currency
      <> " | referenceId="
      <> referenceId
  let (counterparty, ownerId) = case mbFleetOwnerId of
        Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId.getId)
        Nothing -> (counterpartyDriver, driverId.getId)
  res <-
    SLFW.createWalletEntryDelta
      counterparty
      ownerId
      amount
      transporterConfig.currency
      merchantId.getId
      merchantOpCityId.getId
      SLFW.walletReferenceWalletIncentive
      referenceId
      Nothing
  case res of
    Right _ -> pure True
    Left err -> do
      logError $ "Failed to credit driver wallet: " <> show err
      pure False
