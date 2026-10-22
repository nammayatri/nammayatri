{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The wallet floor: the service tier's minimum balance (minWalletBalanceToGoOnline), below which a
--   driver may not be online. It applies in any city where it is enforced ('isMinBalanceEnforced'),
--   wallet-subscription or not; it is the only wallet block (the plan credit limit plays no part).
--
--   'enforceWalletFloorForOwner' runs after the debits that can cross the floor (nightly fee,
--   dues migration, penalties, ledger adjustments). It needs the app environment to change
--   driver mode, so it lives here rather than in the generic post-posting hook.
module SharedLogic.Finance.WalletFloor
  ( WalletFloor (..),
    getWalletFloor,
    isBelowWalletFloor,
    enforceWalletFloorForOwner,
    enforceWalletFloorForDriver,
    afterWalletDebit,
    checkWalletGoOnlineEligibility,
  )
where

import Data.Time (utctDay)
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.Extra.Plan (ServiceNames (..))
import qualified Domain.Types.Mandate as DMandate
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DV
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Account as FAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.WalletAccount
import SharedLogic.Finance.WalletAutopay (ensureAutopayRecharge)
import SharedLogic.Finance.WalletBalanceCache (setDriverWalletBalanceInPool)
import SharedLogic.Finance.WalletSubscription (resolveWalletOwner)
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Mandate as QMandate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error (DriverError (WalletBalanceBelowMinimum), WalletBalanceBelowMinimumPayload (..), WalletBlockReason (..))
import Tools.Notifications (sendNotificationToDriver)

data WalletFloor = WalletFloor
  { requiredBalance :: HighPrecMoney,
    reason :: WalletBlockReason
  }
  deriving (Show, Eq)

-- | The driver's floor, or Nothing when wallet blocking doesn't apply (minimum not enforced in the
--   city, exempt tag, tier without a minimum).
getWalletFloor ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.TransporterConfig ->
  DP.Person ->
  Maybe DV.VehicleVariant ->
  m (Maybe WalletFloor)
getWalletFloor transporterConfig person mbVariant = do
  now <- getCurrentTime
  if not (isMinBalanceEnforced transporterConfig.driverWalletConfig) || isWalletMinBalanceExempt now person.driverTag
    then pure Nothing
    else do
      tiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
      pure $ walletFloorFromTiers transporterConfig now person tiers mbVariant

-- | 'getWalletFloor' for a caller that already holds the city's tiers.
walletFloorFromTiers :: DTC.TransporterConfig -> UTCTime -> DP.Person -> [DVST.VehicleServiceTier] -> Maybe DV.VehicleVariant -> Maybe WalletFloor
walletFloorFromTiers transporterConfig now person tiers mbVariant
  | not (isMinBalanceEnforced transporterConfig.driverWalletConfig) || isWalletMinBalanceExempt now person.driverTag = Nothing
  | otherwise = (`WalletFloor` MIN_BALANCE) <$> (mbVariant >>= tierMinimum tiers)

isBelowWalletFloor :: WalletFloor -> HighPrecMoney -> Bool
isBelowWalletFloor floor' balance = balance < floor'.requiredBalance

-- | Re-check the driver this wallet belongs to and take them offline when below the floor. Only
--   driver wallets matter: the floor is checked on the driver's own wallet, so a posting on a fleet
--   owner's wallet changes nothing for the fleet's drivers.
enforceWalletFloorForOwner ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DTC.TransporterConfig ->
  FAccount.CounterpartyType ->
  Text ->
  m ()
enforceWalletFloorForOwner transporterConfig counterpartyType ownerId =
  when (isMinBalanceEnforced transporterConfig.driverWalletConfig && counterpartyType == FAccount.DRIVER) $
    enforceWalletFloorForDriver transporterConfig counterpartyType ownerId (Id ownerId)

-- | Offline flip for one driver. Only online drivers are touched; a driver on a ride or with an
--   accepted advance booking keeps it -- the pool guard stops new offers.
enforceWalletFloorForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DTC.TransporterConfig ->
  FAccount.CounterpartyType ->
  Text ->
  Id DP.Person ->
  m ()
enforceWalletFloorForDriver transporterConfig counterpartyType ownerId driverId = do
  mbDriverInfo <- QDI.findById (cast driverId)
  whenJust mbDriverInfo $ \driverInfo ->
    when (isOnlineAndFree driverInfo) $ do
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbVehicle <- QVehicle.findById driverId
      mbFloor <- getWalletFloor transporterConfig person ((.variant) <$> mbVehicle)
      whenJust mbFloor $ \floor' -> do
        balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
        setOfflineIfBelowFloor transporterConfig person driverInfo floor' balance

-- | Only online drivers are taken offline; one on a ride or with an accepted advance booking keeps it.
isOnlineAndFree :: DI.DriverInformation -> Bool
isOnlineAndFree driverInfo = driverInfo.mode `elem` [Just DCommon.ONLINE, Just DCommon.SILENT] && not driverInfo.onRide && not driverInfo.hasAdvanceBooking

setOfflineIfBelowFloor ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DTC.TransporterConfig ->
  DP.Person ->
  DI.DriverInformation ->
  WalletFloor ->
  HighPrecMoney ->
  m ()
setOfflineIfBelowFloor transporterConfig person driverInfo floor' balance =
  when (isBelowWalletFloor floor' balance) $ do
    now <- getCurrentTime
    DDriverMode.updateDriverModeAndFlowStatus person.id transporterConfig False (Just DCommon.OFFLINE) DDFS.OFFLINE driverInfo Nothing (Just now)
    logInfo $ "Wallet floor: driver " <> person.id.getId <> " set offline, balance " <> show balance <> " below " <> show floor'.requiredBalance
    let title = "You're offline"
        message = "Your wallet balance is below Rs." <> show floor'.requiredBalance <> ". Recharge your wallet to go online."
    fork "wallet floor offline notification" $
      sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_NOTIFY title message person person.deviceToken

-- | Follow-up to a debit that may have crossed a threshold (nightly fee, dues migration, penalty):
--   take this driver offline if below the floor; then, when the driver's wallet is at or below the
--   trigger level ('autopayTriggerBalance'), schedule an Autopay recharge (wallet-subscription cities
--   only) or nudge the driver to recharge, at most once a day.
afterWalletDebit ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DTC.TransporterConfig ->
  DP.Person ->
  m ()
afterWalletDebit transporterConfig driver =
  when (walletSubscription || isMinBalanceEnforced dwc) $ do
    -- Balance, vehicle and tiers are read once and serve both the floor check and the recharge/nudge.
    (counterpartyType, ownerId) <- resolveWalletOwner driver.id
    balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
    mbVariant <- fmap (.variant) <$> QVehicle.findById driver.id
    tiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId Nothing
    floorNow <- getCurrentTime
    whenJust (walletFloorFromTiers transporterConfig floorNow driver tiers mbVariant) $ \floor' ->
      when (isBelowWalletFloor floor' balance) $ do
        mbDriverInfo <- QDI.findById (cast driver.id)
        whenJust mbDriverInfo $ \driverInfo ->
          when (isOnlineAndFree driverInfo) $ setOfflineIfBelowFloor transporterConfig driver driverInfo floor' balance
    let mbMinimum = mbVariant >>= tierMinimum tiers
        -- Recharge and nudge start at the autopay trigger (above the blocking minimum), so the driver
        -- is topped up before they would be blocked.
        triggerLevel = autopayTriggerBalance dwc tiers mbVariant
    when (balance <= triggerLevel) $ do
      -- Autopay only recharges the wallet in wallet-subscription cities (elsewhere the mandate pays
      -- the subscription dues), so this is Nothing there.
      mbRecharge <- ensureAutopayRecharge transporterConfig driver mbVariant balance
      now <- getCurrentTime
      isFirstToday <- Redis.setNxExpire (lowBalanceNudgeKey driver.id now) (24 * 3600) True
      when isFirstToday $ do
        let enforcedLine = case mbMinimum of
              Just minBalance | isMinBalanceEnforced dwc -> " Minimum Rs." <> show minBalance <> " needed to go online."
              _ -> ""
            rechargeHint = if walletSubscription then "Recharge your wallet or set up Autopay." else "Recharge your wallet."
            message = case mbRecharge of
              Just recharge -> "Your wallet balance is Rs." <> show balance <> ". Rs." <> show recharge.platformFee.fee <> " will be added by Autopay." <> enforcedLine
              Nothing -> "Your wallet balance is Rs." <> show balance <> ". " <> rechargeHint <> enforcedLine
        fork "wallet low balance nudge" $
          sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_NOTIFY "Low wallet balance" message driver driver.deviceToken
  where
    dwc = transporterConfig.driverWalletConfig
    walletSubscription = isWalletSubscriptionCity dwc
    lowBalanceNudgeKey driverId now = "WalletLowBalanceNudge:" <> driverId.getId <> ":" <> show (utctDay now)

-- | Go-online check (cities enforcing the minimum): the driver's live available balance must be at
--   least the floor. It also refreshes the pool's cached balance (the raw balance, as every other
--   writer of that cache uses). A blocked driver with a mandate in a wallet-subscription city gets a
--   recharge scheduled before the error is raised, so a driver who didn't ride yesterday (and so had
--   no nightly trigger) still gets one.
checkWalletGoOnlineEligibility ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTC.TransporterConfig ->
  DP.Person ->
  Maybe DV.VehicleVariant ->
  m ()
checkWalletGoOnlineEligibility transporterConfig person mbVariant =
  when (isMinBalanceEnforced transporterConfig.driverWalletConfig) $ do
    (counterpartyType, ownerId) <- resolveWalletOwner person.id
    balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
    fork "refresh pool wallet balance on go-online" $ do
      rawBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterpartyType ownerId
      setDriverWalletBalanceInPool person.id rawBalance
    mbFloor <- getWalletFloor transporterConfig person mbVariant
    whenJust mbFloor $ \floor' ->
      when (isBelowWalletFloor floor' balance) $ do
        -- Scheduling the recharge is best effort here: if it fails, the driver still gets the 402 that
        -- tells them to recharge, not a server error.
        mbRecharge <-
          withTryCatch "checkWalletGoOnlineEligibility:ensureAutopayRecharge" (ensureAutopayRecharge transporterConfig person mbVariant balance) >>= \case
            Right recharge -> pure recharge
            Left err -> do
              logError $ "Autopay recharge could not be scheduled at go-online for driver " <> person.id.getId <> ": " <> show err
              pure Nothing
        mbDriverPlan <- QDPlan.findByDriverIdWithServiceName person.id YATRI_SUBSCRIPTION
        -- "Autopay is active" needs a mandate that is itself active, not just the plan flag: a plan can read
        -- ACTIVE with no mandate, or with one that is not usable. A mandate row that can't be read counts as none.
        mbMandate <- case mbDriverPlan >>= (.mandateId) of
          Nothing -> pure Nothing
          Just mandateId ->
            withTryCatch "checkWalletGoOnlineEligibility:mandate" (QMandate.findById mandateId) >>= \case
              Right mandate -> pure mandate
              Left _ -> pure Nothing
        throwError $
          WalletBalanceBelowMinimum
            WalletBalanceBelowMinimumPayload
              { balance = balance,
                requiredBalance = floor'.requiredBalance,
                shortfall = max 0 (floor'.requiredBalance - balance),
                reason = floor'.reason,
                -- Autopay refills the wallet only in wallet-subscription cities.
                autopayActive = isWalletSubscriptionCity transporterConfig.driverWalletConfig && (mbDriverPlan >>= (.autoPayStatus)) == Just DI.ACTIVE && (mbMandate <&> (.status)) == Just DMandate.ACTIVE,
                nextAutopayRechargeAt = (.payBy) <$> mbRecharge
              }
