{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The wallet floor: the balance below which a driver in a wallet-subscription city may not be
--   online. Before GST is mandatory it is today's dues limit (−plan credit limit); from then on
--   it is the service tier's positive minimum.
--
--   'enforceWalletFloorForOwner' runs after the debits that can cross the floor (nightly fee,
--   end ride, dues migration, ledger adjustments). It needs the app environment to change driver
--   mode, so it lives here rather than in the generic post-posting hook.
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
import qualified Storage.CachedQueries.Plan as CQP
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

-- | The driver's floor, or Nothing when wallet blocking doesn't apply (not a wallet city,
--   exempt tag, tier without a minimum, plan that doesn't block on dues, no plan).
getWalletFloor ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.TransporterConfig ->
  DP.Person ->
  Maybe DV.VehicleVariant ->
  m (Maybe WalletFloor)
getWalletFloor transporterConfig person mbVariant = do
  now <- getCurrentTime
  let dwc = transporterConfig.driverWalletConfig
  if not (isWalletSubscriptionCity dwc) || isWalletMinBalanceExempt now person.driverTag
    then pure Nothing
    else
      if isMinBalanceEnforced dwc
        then do
          tiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
          pure $ (`WalletFloor` MIN_BALANCE) <$> (mbVariant >>= tierMinimum tiers)
        else do
          -- Same limit and toggle the postpaid nightly job blocks on today.
          mbDriverPlan <- QDPlan.findByDriverIdWithServiceName person.id YATRI_SUBSCRIPTION
          mbPlan <- maybe (pure Nothing) (\dp -> CQP.findByIdAndPaymentModeWithServiceName dp.planId dp.planType YATRI_SUBSCRIPTION) mbDriverPlan
          pure $ do
            plan <- mbPlan
            guard plan.subscribedFlagToggleAllowed
            pure $ WalletFloor (negate plan.maxCreditLimit) DUES_LIMIT

-- | Today's dues rule blocks at dues >= limit, i.e. balance <= −limit; the minimum blocks below it.
isBelowWalletFloor :: WalletFloor -> HighPrecMoney -> Bool
isBelowWalletFloor floor' balance = case floor'.reason of
  MIN_BALANCE -> balance < floor'.requiredBalance
  DUES_LIMIT -> balance <= floor'.requiredBalance

-- | Re-check the driver this wallet belongs to and take them offline when below the floor. Only
--   driver wallets matter: the charges of this feature settle in the driver's own wallet, so a posting
--   on a fleet owner's wallet changes nothing for the fleet's drivers.
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
  when (isWalletSubscriptionCity transporterConfig.driverWalletConfig && counterpartyType == FAccount.DRIVER) $
    enforceWalletFloorForDriver transporterConfig counterpartyType ownerId (Id ownerId)

-- | Offline flip for one driver. Only online drivers are touched; a driver on a ride or with an
--   accepted advance booking keeps it -- the pool guard stops new offers, and the end-ride path
--   calls this again once the ride is over.
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
    when (driverInfo.mode `elem` [Just DCommon.ONLINE, Just DCommon.SILENT] && not driverInfo.onRide && not driverInfo.hasAdvanceBooking) $ do
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbVehicle <- QVehicle.findById driverId
      mbFloor <- getWalletFloor transporterConfig person ((.variant) <$> mbVehicle)
      whenJust mbFloor $ \floor' -> do
        balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
        when (isBelowWalletFloor floor' balance) $ do
          now <- getCurrentTime
          DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig False (Just DCommon.OFFLINE) DDFS.OFFLINE driverInfo Nothing (Just now)
          logInfo $ "Wallet floor: driver " <> driverId.getId <> " set offline, balance " <> show balance <> " below " <> show floor'.requiredBalance
          let title = "You're offline"
              message = "Your wallet balance is below Rs." <> show floor'.requiredBalance <> ". Recharge your wallet to go online."
          fork "wallet floor offline notification" $
            sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_NOTIFY title message person person.deviceToken

-- | Follow-up to a debit that may have crossed a threshold (nightly fee, dues migration): take
--   this driver offline if below the floor; then, when the driver's wallet is at or below the
--   trigger level (tier minimum, or 0), schedule an Autopay recharge, or -- without an active
--   mandate -- nudge the driver to recharge, at most once a day.
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
  when (isWalletSubscriptionCity transporterConfig.driverWalletConfig) $ do
    (counterpartyType, ownerId) <- resolveWalletOwner driver.id
    enforceWalletFloorForDriver transporterConfig counterpartyType ownerId driver.id
    when (counterpartyType == FAccount.DRIVER) $ do
      balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
      mbVariant <- fmap (.variant) <$> QVehicle.findById driver.id
      tiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId Nothing
      let mbMinimum = mbVariant >>= tierMinimum tiers
          -- Recharge and nudge start at the autopay trigger (above the blocking minimum), so the driver
          -- is topped up before they would be blocked.
          triggerLevel = autopayTriggerBalance transporterConfig.driverWalletConfig tiers mbVariant
      when (balance <= triggerLevel) $ do
        mbRecharge <- ensureAutopayRecharge transporterConfig driver mbVariant balance
        now <- getCurrentTime
        isFirstToday <- Redis.setNxExpire (lowBalanceNudgeKey driver.id now) (24 * 3600) True
        when isFirstToday $ do
          let enforcedLine = case mbMinimum of
                Just minBalance | isMinBalanceEnforced transporterConfig.driverWalletConfig -> " Minimum Rs." <> show minBalance <> " needed to go online."
                _ -> ""
              message = case mbRecharge of
                Just recharge -> "Your wallet balance is Rs." <> show balance <> ". Rs." <> show recharge.platformFee.fee <> " will be added by Autopay." <> enforcedLine
                Nothing -> "Your wallet balance is Rs." <> show balance <> ". Recharge your wallet or set up Autopay." <> enforcedLine
          fork "wallet low balance nudge" $
            sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_NOTIFY "Low wallet balance" message driver driver.deviceToken
  where
    lowBalanceNudgeKey driverId now = "WalletLowBalanceNudge:" <> driverId.getId <> ":" <> show (utctDay now)

-- | Go-online check (wallet-subscription cities): the wallet owner's live balance must be above
--   the floor. The live read is the source of truth, so it also refreshes the pool's cached
--   balance. A blocked driver with a mandate gets a recharge scheduled before the error is raised,
--   so a driver who didn't ride yesterday (and so had no nightly trigger) still gets one.
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
  when (isWalletSubscriptionCity transporterConfig.driverWalletConfig) $ do
    (counterpartyType, ownerId) <- resolveWalletOwner person.id
    balance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner counterpartyType ownerId
    fork "refresh pool wallet balance on go-online" $ setDriverWalletBalanceInPool person.id balance
    mbFloor <- getWalletFloor transporterConfig person mbVariant
    whenJust mbFloor $ \floor' ->
      when (isBelowWalletFloor floor' balance) $ do
        -- Scheduling the recharge is best effort here: if it fails, the driver still gets the 402 that
        -- tells them to recharge, not a server error.
        mbRecharge <-
          if counterpartyType == FAccount.DRIVER
            then
              withTryCatch "checkWalletGoOnlineEligibility:ensureAutopayRecharge" (ensureAutopayRecharge transporterConfig person mbVariant balance) >>= \case
                Right recharge -> pure recharge
                Left err -> do
                  logError $ "Autopay recharge could not be scheduled at go-online for driver " <> person.id.getId <> ": " <> show err
                  pure Nothing
            else pure Nothing
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
                autopayActive = (mbDriverPlan >>= (.autoPayStatus)) == Just DI.ACTIVE && (mbMandate <&> (.status)) == Just DMandate.ACTIVE,
                nextAutopayRechargeAt = (.payBy) <$> mbRecharge
              }
