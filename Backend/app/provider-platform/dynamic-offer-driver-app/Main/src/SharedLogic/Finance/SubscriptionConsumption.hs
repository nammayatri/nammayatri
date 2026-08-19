{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Prepaid subscription consumption on cancellation.
--
-- When a rider is charged a cancellation fee the driver is compensated, so the ride slot is
-- consumed and their prepaid credit is debited — at cancellation, not at settlement.
--
-- This cannot reuse the ride-end path. 'debitPrepaidBalance' works by settling a hold created
-- at ride start, and @releaseLien@ has already voided that hold before the fee is computed
-- (Beckn/Cancel.hs:123, CancelRide/Internal.hs:291). Hence 'debitPrepaidBalanceDirect'.
--
-- The revenue-amount and allocation-metadata helpers below duplicate logic still living as
-- @where@-bound closures inside @EndRide/Internal.processEndRideFinance@. They were written
-- here rather than extracted so the hot ride-end path stays untouched; pointing EndRide at
-- these and deleting its private copies is the intended follow-up.
module SharedLogic.Finance.SubscriptionConsumption
  ( consumeCancellationRideCredit,
  )
where

import qualified Data.Map as M
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Booking as SRB
import Domain.Types.Extra.Plan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleVariant as Variant
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntryMetadata (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import SharedLogic.Allocator
import SharedLogic.DriverFee (calculatePlatformFeeAttr)
import SharedLogic.Finance.Prepaid
import SharedLogic.Ride (makeSubscriptionRunningBalanceLockKey)
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPE
import Tools.Error

type ConsumptionFlow m r =
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    Finance.HasActorInfo m r
  )

-- | Debit the driver's (or fleet owner's) prepaid ride credit for a charged cancellation.
--
-- No-op when the owner has no active prepaid subscription, so it is safe to call
-- unconditionally for merchants not on prepaid.
consumeCancellationRideCredit ::
  ConsumptionFlow m r =>
  SRB.Booking ->
  DRide.Ride ->
  HighPrecMoney -> -- cancellation base, tax-exclusive
  DTC.TransporterConfig ->
  m ()
consumeCancellationRideCredit booking ride consumeAmount transporterConfig
  | consumeAmount <= 0 = pure ()
  | otherwise = do
    let vehicleCategoryScoped = fromMaybe False transporterConfig.subscriptionConfig.vehicleCategoryScopedPrepaidEnabled
        mbVehicleCategory =
          if vehicleCategoryScoped
            then Just (Variant.castServiceTierToVehicleCategory booking.vehicleServiceTier)
            else Nothing
    (ownerType, ownerId, counterpartyType) <- resolveOwner
    Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey ownerId) 10 10 $ do
      mbPurchase <- QSPE.findLatestActiveByOwnerAndServiceName handleSubscriptionExpiry ownerId ownerType PREPAID_SUBSCRIPTION mbVehicleCategory
      case mbPurchase of
        Nothing -> logInfo $ "consumeCancellationRideCredit: no active prepaid subscription for owner " <> ownerId
        Just purchase -> do
          revenueAmount <- prepaidRevenueAmount purchase consumeAmount
          mbMetadata <- allocationMetadata counterpartyType ownerId ownerType consumeAmount mbVehicleCategory
          _ <-
            debitPrepaidBalanceDirect
              counterpartyType
              ownerId
              consumeAmount
              revenueAmount
              booking.currency
              booking.providerId.getId
              booking.merchantOperatingCityId.getId
              booking.id.getId
              mbMetadata
              mbVehicleCategory
              >>= fromEitherM (\err -> InternalError ("Failed to debit prepaid balance on cancellation: " <> show err))
          (contributingPurchaseIds, anyExhausted) <- checkAndMarkExhaustedSubscriptions counterpartyType ownerId ownerType mbVehicleCategory
          unless (null contributingPurchaseIds) $
            QRide.updateSubscriptionPurchaseIds (Just contributingPurchaseIds) ride.id
          -- Must activate here. checkAndMarkExhaustedSubscriptions only reads ACTIVE purchases, so
          -- once this one is marked EXHAUSTED no later call reports it again and anyExhausted stays
          -- False forever — a queued purchase would never activate and the driver would hold credit
          -- they cannot spend.
          when anyExhausted $ do
            mbActivated <- activateNextQueuedPurchaseExpiry ownerId ownerType mbVehicleCategory
            whenJust mbActivated $ \(nextPurchaseId, expiry) -> do
              now <- getCurrentTime
              let delay = diffUTCTime expiry now
              createJobIn @_ @'ExpireSubscriptionPurchase
                (Just booking.providerId)
                (Just booking.merchantOperatingCityId)
                delay
                $ ExpireSubscriptionPurchaseJobData
                  { subscriptionPurchaseId = nextPurchaseId
                  }
          logInfo $ "consumeCancellationRideCredit: consumed " <> show consumeAmount <> " for owner " <> ownerId <> " on booking " <> booking.id.getId
  where
    resolveOwner = case ride.fleetOwnerId of
      Just fleetOwnerId -> pure (DSP.FLEET_OWNER, fleetOwnerId.getId, counterpartyFleetOwner)
      Nothing -> do
        person <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        if DCommon.checkFleetOwnerRole person.role
          then pure (DSP.FLEET_OWNER, person.id.getId, counterpartyFleetOwner)
          else pure (DSP.DRIVER, person.id.getId, counterpartyDriver)

    -- Revenue recognised for this consumption, pro-rated by the plan's platform fee.
    prepaidRevenueAmount purchase amount = do
      let syntheticPlan = Plan.mkSyntheticDriverPlanFromPurchase purchase
      mbPlan <- CQP.findByIdAndPaymentModeWithServiceName syntheticPlan.planId syntheticPlan.planType PREPAID_SUBSCRIPTION
      case mbPlan of
        Nothing -> pure 0
        Just plan_ -> do
          let (fee, _cgst, _sgst) = calculatePlatformFeeAttr plan_.registrationAmount plan_
              totalCredit = case plan_.planBaseAmount of
                PERRIDE_BASE a -> a
                DAILY_BASE a -> a
                WEEKLY_BASE a -> a
                MONTHLY_BASE a -> a
                RECHARGE_BASE a -> a
          pure $ if totalCredit > 0 then (amount * fee) / totalCredit else 0

    -- FIFO split across active subscriptions, so the debit is attributable per purchase.
    allocationMetadata counterpartyType ownerId ownerType amount mbVC = do
      allActive <- QSPE.findAllActiveByOwnerAndServiceName ownerId ownerType PREPAID_SUBSCRIPTION mbVC
      mbBalance <- getPrepaidBalanceByOwner counterpartyType ownerId mbVC
      let balanceBefore = fromMaybe 0 mbBalance
          sortedActive = sortOn (.purchaseTimestamp) allActive
          allocations = computeFifoSubscriptionAllocations amount balanceBefore sortedActive
      pure $
        if null allocations
          then Nothing
          else
            Just
              LedgerEntryMetadata
                { subscriptionAllocations = Just allocations,
                  reason = Nothing,
                  driverPayable = Nothing,
                  payoutOrderId = Nothing,
                  d2cReferralEarnings = Nothing,
                  d2dReferralEarnings = Nothing,
                  dailyStatsId = Nothing
                }
