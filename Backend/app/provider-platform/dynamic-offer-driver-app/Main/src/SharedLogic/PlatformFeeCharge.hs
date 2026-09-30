{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Wallet-settled platform fee (fare policy @platformFeeChargesBy = WalletCharged@): what is
--   owed, and how it posts. Composed into a ride by 'SharedLogic.RideWalletCharges'.
--
--   The amount itself already rides on 'FareParameters' — 'SharedLogic.FareCalculator' copies the
--   fare policy's @platformFee/cgst/sgst@ onto every fare params regardless of settlement method,
--   and the flat (non-slab) platform fee is deliberately excluded from 'fareSum', so the customer
--   never pays it. This module only decides /whether/ to charge it.
module SharedLogic.PlatformFeeCharge
  ( platformFeeWalletCharge,
  )
where

import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Person as DP
import Domain.Types.Plan (ServiceNames (..))
import Domain.Types.TransporterConfig (TransporterConfig)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, logDebug)
import Lib.Finance (AccountRole (..), FinanceM, transfer)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.DriverOnboarding (getFreeTrialDaysLeft)
import qualified SharedLogic.Finance.Wallet as Wallet
import SharedLogic.Finance.WalletCharge (WalletCharge (..))
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverPlan as QDPlan

type PlatformFeeFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    Esq.EsqDBFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r
  )

-- | The platform fee as one wallet charge: what it debits (zero unless it applies) and how it
--   posts. Contributes no balance floor — the fee is a cost, not a policy minimum.
platformFeeWalletCharge ::
  forall m r.
  PlatformFeeFlow m r =>
  TransporterConfig ->
  DI.DriverInformation ->
  Id DP.Person ->
  DFare.FareParameters ->
  m (WalletCharge m)
platformFeeWalletCharge transporterConfig driverInfo driverId fareParams = do
  applies <- feeApplies transporterConfig driverInfo driverId fareParams
  -- cgst and sgst are explicit on the fare policy, so unlike the airport entry fee there is no
  -- GST to back out: the base is the fee as configured.
  let (base, gst)
        | not applies = (0, 0)
        | otherwise =
          ( fromMaybe 0 fareParams.platformFee,
            fromMaybe 0 fareParams.cgst + fromMaybe 0 fareParams.sgst
          )
  pure
    WalletCharge
      { label = "PlatformFee",
        debitAmount = base + gst,
        minBalanceFloor = Nothing,
        postLegs = postLegs' base gst
      }
  where
    -- Base to SellerRevenue (platform revenue, like commission — not a third-party pass-through),
    -- GST to GovtIndirect. Runs inside the caller's FinanceM block so every charge on the ride
    -- lands in one atomic posting.
    postLegs' :: HighPrecMoney -> HighPrecMoney -> FinanceM m ()
    postLegs' base gst = do
      when (base > 0) $
        void $ transfer OwnerLiability SellerRevenue base Wallet.walletReferencePlatformFee Nothing
      when (gst > 0) $
        void $ transfer OwnerLiability GovtIndirect gst Wallet.walletReferencePlatformFeeGST Nothing

-- | Whether a wallet-settled platform fee should be charged for this ride.
--
--   'DFP.WalletCharged' is a settlement-channel swap for a charge that already exists — it must
--   not introduce a charge where no platform fee is levied today. So every exemption that
--   'createDriverFee' applies is reproduced here:
--
--   * the fare policy must elect 'DFP.WalletCharged';
--   * the city must run subscriptions at all (@transporterConfig.subscription@);
--   * the driver must be past their free trial, unless the city charges special-zone rides during
--     free trial. 'DFP.WalletCharged' counts as a special-zone charge for this purpose, which
--     preserves current behaviour for the 'DFP.FixedAmount' policies being converted.
--
--   Guards are ordered cheapest first: a ride on an unconverted fare policy costs no queries.
feeApplies ::
  (MonadFlow m, CacheFlow m r, Esq.EsqDBFlow m r) =>
  TransporterConfig ->
  DI.DriverInformation ->
  Id DP.Person ->
  DFare.FareParameters ->
  m Bool
feeApplies transporterConfig driverInfo driverId fareParams
  -- Every branch that declines logs why. These are all silent config gates; without a line each,
  -- a fee that quietly never charges is indistinguishable from one that is broken.
  | fareParams.platformFeeChargesBy /= DFP.WalletCharged =
    skip $ "fare policy platformFeeChargesBy is " <> show fareParams.platformFeeChargesBy <> ", not WalletCharged"
  | not transporterConfig.subscription = skip "transporterConfig.subscription is off for this city"
  | otherwise = do
    onFreeTrial <- isOnFreeTrial
    if onFreeTrial && not transporterConfig.considerSpecialZoneRideChargesInFreeTrial
      then skip "driver is on free trial and considerSpecialZoneRideChargesInFreeTrial is off"
      else do
        logDebug $ "platformFeeWalletCharge: charging, driverId: " <> driverId.getId <> ", platformFee: " <> show fareParams.platformFee <> ", cgst: " <> show fareParams.cgst <> ", sgst: " <> show fareParams.sgst
        pure True
  where
    skip reason = do
      logDebug $ "platformFeeWalletCharge: skipping - " <> reason <> ", driverId: " <> driverId.getId
      pure False

    -- Mirrors getPlanAndPushToDefualtIfEligible in EndRide.Internal, including its default:
    -- with no subscription config the driver is treated as being on free trial, i.e. exempt.
    isOnFreeTrial = do
      mbSubsConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName transporterConfig.merchantOperatingCityId Nothing YATRI_SUBSCRIPTION
      case mbSubsConfig of
        Nothing -> pure True
        Just subsConfig -> do
          freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
          mbDriverPlan <- QDPlan.findByDriverIdWithServiceName (cast driverId) YATRI_SUBSCRIPTION
          fst <$> Plan.isOnFreeTrial driverId subsConfig freeTrialDaysLeft mbDriverPlan
