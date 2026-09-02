{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Phase D of the cancellation unification (dev/docs/cancellation-consequence-matrix-plan.md):
-- ONE dimensioned table decides every consequence of a cancellation — customer money,
-- driver coins, driver penalty, collection mode, side effects — keyed on the fault
-- verdict and its rule name. The matrix is AUTHORITATIVE: no matching row means no
-- consequences (logged); there is no fallback to the old JsonLogic amount rules.
--
-- Resolution is most-specific-wins over nullable dimensions (null = wildcard) with fixed
-- weights so ties are impossible between rows with different dimension sets; equal-score
-- rows (a validation failure) resolve deterministically by lowest id.
module SharedLogic.CancellationConsequence where

import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationConsequenceMatrix as DCCM
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Extra.CancellationConsequenceMatrix as DExtra
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions (createWithKV, updateOneWithKV)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Id
import qualified Kernel.Types.SystemConfigs as KTSC
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT2
import qualified Lib.Types.SpecialLocation as SL
import qualified Sequelize as Se
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified Storage.CachedQueries.CancellationConsequenceMatrix as CQCCM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import Tools.Error

data ConsequenceInput = ConsequenceInput
  { merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    faultVerdict :: Maybe CancellationFault.FaultVerdict,
    cancelledBy :: DCT2.CancellationType,
    tripCategory :: DTC.TripCategory,
    vehicleServiceTier :: DTC.ServiceTierType,
    area :: Maybe SL.Area,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    isDashboardBooking :: Bool
  }
  deriving (Generic, Show)

-- | Resolve once per ride and cache, so the charge calculation (main cancel flow) and
-- the coin event (fork) apply the SAME row. The main flow resolves first with the full
-- context (area, payment instrument); the fork then reads the cached decision.
getOrResolveConsequence :: (CacheFlow m r, EsqDBFlow m r) => Id DRide.Ride -> ConsequenceInput -> m (Maybe DCCM.CancellationConsequenceMatrix)
getOrResolveConsequence rideId input = do
  mbCached :: Maybe DCCM.CancellationConsequenceMatrix <- Redis.safeGet (consequenceRowKey rideId)
  case mbCached of
    Just row -> pure (Just row)
    Nothing -> do
      mbRow <- resolveConsequence input
      whenJust mbRow $ \row -> Redis.setExp (consequenceRowKey rideId) row consequenceRowTtl
      pure mbRow

consequenceRowKey :: Id DRide.Ride -> Text
consequenceRowKey rideId = "CancellationConsequence:rideId-" <> rideId.getId

consequenceRowTtl :: Int
consequenceRowTtl = 3600

resolveConsequence :: (CacheFlow m r, EsqDBFlow m r) => ConsequenceInput -> m (Maybe DCCM.CancellationConsequenceMatrix)
resolveConsequence input = do
  rows <- CQCCM.findAllByMerchantOpCityId input.merchantOperatingCityId
  let eventVerdict = input.faultVerdict <&> \v -> show v.atFault
      eventRule = (.rule) <$> input.faultVerdict
      matches row =
        row.active
          && dimMatches row.faultVerdict eventVerdict
          && dimMatches row.faultRule eventRule
          && dimMatches row.cancelledBy (Just input.cancelledBy)
          && dimMatches row.tripCategory (Just input.tripCategory)
          && dimMatches row.vehicleServiceTier (Just input.vehicleServiceTier)
          && dimMatches row.area input.area
          && dimMatches row.paymentInstrument input.paymentInstrument
      candidates = filter matches rows
      mbWinner = listToMaybe $ sortOn (\r -> (Down (specificity r), r.id.getId)) candidates
  case mbWinner of
    Nothing -> do
      logError $ "CancellationConsequenceMatrix miss: no row matched for city " <> input.merchantOperatingCityId.getId <> ", verdict=" <> show eventVerdict <> ", rule=" <> show eventRule <> ", cancelledBy=" <> show input.cancelledBy <> " — NO consequences applied"
      pure Nothing
    Just row
      | input.isDashboardBooking && row.exemptDashboardBookings -> do
        logInfo $ "CancellationConsequenceMatrix: row " <> row.id.getId <> " exempts dashboard bookings — no consequences applied"
        pure Nothing
      | otherwise -> pure (Just row)

-- null dimension on the row = wildcard; a set dimension requires the event value to match
-- (an absent event value does NOT match a set dimension)
dimMatches :: Eq a => Maybe a -> Maybe a -> Bool
dimMatches Nothing _ = True
dimMatches (Just rowVal) (Just eventVal) = rowVal == eventVal
dimMatches (Just _) Nothing = False

-- fixed weights: faultRule > faultVerdict > cancelledBy > tripCategory > vehicleServiceTier > area/paymentInstrument
specificity :: DCCM.CancellationConsequenceMatrix -> Int
specificity row =
  sum
    [ 32 * fromEnum (isJust row.faultRule),
      16 * fromEnum (isJust row.faultVerdict),
      8 * fromEnum (isJust row.cancelledBy),
      4 * fromEnum (isJust row.tripCategory),
      2 * fromEnum (isJust row.vehicleServiceTier),
      1 * fromEnum (isJust row.area),
      1 * fromEnum (isJust row.paymentInstrument)
    ]

-- | Build the resolver input from a full booking — the ONE place the dimension values
-- come from, so every resolution site (charge calc, side effects, coin fork when it has
-- the full booking) produces identical inputs and the per-ride cache stays coherent.
buildConsequenceInputFromBooking :: (CacheFlow m r, EsqDBFlow m r) => SRB.Booking -> Maybe CancellationFault.FaultVerdict -> DCT2.CancellationType -> m ConsequenceInput
buildConsequenceInputFromBooking booking mbFaultVerdict cancelledBy = do
  mbPaymentMethod <- forM booking.paymentMethodId $ \pmId ->
    CQMPM.findByIdAndMerchantOpCityId pmId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound pmId.getId)
  -- a booking without a payment method is treated as Cash
  let bookingPaymentInstrument = maybe DMPM.Cash (.paymentInstrument) mbPaymentMethod
  pure
    ConsequenceInput
      { merchantOperatingCityId = booking.merchantOperatingCityId,
        faultVerdict = mbFaultVerdict,
        cancelledBy = cancelledBy,
        tripCategory = booking.tripCategory,
        vehicleServiceTier = booking.vehicleServiceTier,
        area = booking.area,
        paymentInstrument = Just bookingPaymentInstrument,
        isDashboardBooking = booking.isDashboardRequest
      }

data CustomerChargeBreakup = CustomerChargeBreakup
  { fee :: Maybe HighPrecMoney,
    tax :: Maybe HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    overdueFee :: Maybe HighPrecMoney
  }
  deriving (Generic, Show)

-- | Customer money from the row's customerDeduction. Direction is explicit in the
-- constructor (amounts always positive):
--
--   * MoneyDeduction — charge: FixedMoney carries the amount and an optional overdue
--     amount; PercentageMoney is % of estimated fare clamped to [minAmount, maxAmount]
--     (max cap wins a misconfigured min > max). Tax is always a percentage of the base;
--     commission is fixed or a percentage of the base.
--   * MoneyAddition — credit: reduces the customer's outstanding cancellation dues
--     (clamped at zero downstream, no dues row is created). Internally represented as a
--     negative fee; tax/commission/overdue never apply to credits.
--   * Coin variants on the customer side are meaningless (riders have no coin wallet)
--     and yield no charge.
computeCustomerCharge :: DCCM.CancellationConsequenceMatrix -> HighPrecMoney -> CustomerChargeBreakup
computeCustomerCharge row estimatedFare =
  case row.customerDeduction of
    Just (DExtra.MoneyDeduction money) ->
      let (amount, overdueFee) = moneyDeductionAmount money estimatedFare
          (base, tax) =
            if amountsAreInclusiveOfTax row
              then splitTaxInclusiveTotal row amount
              else (amount, fst (customerTaxAndCommission row amount))
          commission = snd (customerTaxAndCommission row base)
       in CustomerChargeBreakup {fee = Just base, tax, commission, overdueFee}
    Just (DExtra.MoneyAddition money) ->
      let (base, _overdue) = moneyDeductionAmount money estimatedFare
       in CustomerChargeBreakup {fee = Just (negate (abs base)), tax = Nothing, commission = Nothing, overdueFee = Nothing}
    _ -> CustomerChargeBreakup Nothing Nothing Nothing Nothing

shouldCarryForwardDues :: Maybe DCCM.CancellationConsequenceMatrix -> Bool
shouldCarryForwardDues mbRow = (mbRow >>= (.collectionMode)) /= Just DCCM.ImmediateCapture

amountsAreInclusiveOfTax :: DCCM.CancellationConsequenceMatrix -> Bool
amountsAreInclusiveOfTax row = fromMaybe False ((.amountsInclusiveOfTax) =<< row.customerCommissionAndTax)

customerIsCharged :: DCCM.CancellationConsequenceMatrix -> Bool
customerIsCharged row = case row.customerDeduction of
  Just (DExtra.MoneyDeduction _) -> True
  _ -> False

-- | Split a TAX-INCLUSIVE total back into (base, tax) using the row's tax percentage.
-- ride.cancellationFeeIfCancelled stores the total quoted to the rider at soft-cancel
-- (base + tax, see API.Beckn.Cancel), so the confirm path must divide it out rather
-- than apply tax to it a second time.
splitTaxInclusiveTotal :: DCCM.CancellationConsequenceMatrix -> HighPrecMoney -> (HighPrecMoney, Maybe HighPrecMoney)
splitTaxInclusiveTotal row total
  | not (customerIsCharged row) = (total, Nothing)
  | otherwise =
    case (.taxPercentage) =<< row.customerCommissionAndTax of
      Just p | 100 + p /= 0 -> let base = total * 100 / (100 + p) in (base, Just (total - base))
      _ -> (total, Nothing)

-- | Tax and commission for a base charge. Split out of 'computeCustomerCharge' because
-- the confirm-cancel path has to rebuild them against the fee quoted at soft-cancel
-- rather than the fee the row would compute now.
customerTaxAndCommission :: DCCM.CancellationConsequenceMatrix -> HighPrecMoney -> (Maybe HighPrecMoney, Maybe HighPrecMoney)
customerTaxAndCommission row base
  | not (customerIsCharged row) = (Nothing, Nothing)
  | otherwise =
    ( ((.taxPercentage) =<< row.customerCommissionAndTax) <&> \p -> base * p / 100,
      ((.commission) =<< row.customerCommissionAndTax) <&> \case
        DExtra.FixedRate {amount} -> amount
        DExtra.PercentageRate {percentage} -> base * percentage / 100
    )

moneyDeductionAmount :: DExtra.MoneyDeduction -> HighPrecMoney -> (HighPrecMoney, Maybe HighPrecMoney)
moneyDeductionAmount money estimatedFare = case money of
  DExtra.FixedMoney {amount, overdueAmount} -> (amount, overdueAmount)
  DExtra.PercentageMoney {percentage, minAmount, maxAmount} ->
    let raw = percentage * estimatedFare / 100
        floored = maybe raw (max raw) minAmount
     in (maybe floored (min floored) maxAmount, Nothing)

-- | Driver coins from the row (Nothing when the driver consequence is money or absent).
-- The matrix stores a POSITIVE count with the direction in the constructor; the coin
-- engine takes a signed delta, so this adapter negates deductions. abs() guards against
-- any row that slipped past validation with a signed count.
driverCoinDeduction :: DCCM.CancellationConsequenceMatrix -> Maybe (Int, Maybe Int)
driverCoinDeduction row =
  row.driverDeduction >>= \case
    DExtra.CoinDeduction {coins, expirySeconds} -> Just (negate (abs coins), expirySeconds)
    DExtra.CoinAddition {coins, expirySeconds} -> Just (abs coins, expirySeconds)
    _ -> Nothing

-- | Could a driver-initiated cancel in this city cost the driver money? Used for overlay
-- copy (FEE_APPLIES vs FREE_CANCEL) at pickup-progress time, where the fault verdict is
-- not known yet: True when ANY active row a driver cancel could match (cancelledBy
-- wildcard or CancellationByDriver) carries a positive MONEY driver deduction. Replaces
-- the retired farePolicy.driverCancellationPenaltyAmount fare-params snapshot.
cityHasDriverCancelMoneyPenalty :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> HighPrecMoney -> m Bool
cityHasDriverCancelMoneyPenalty merchantOpCityId estimatedFare = do
  rows <- CQCCM.findAllByMerchantOpCityId merchantOpCityId
  pure $ any (\r -> r.active && dimMatches r.cancelledBy (Just DCT2.CancellationByDriver) && maybe False (> 0) (driverMoneyDeduction r estimatedFare)) rows

-- | Driver money from the row (Nothing when the driver consequence is coins or absent).
-- The matrix stores POSITIVE amounts with the direction in the constructor; downstream
-- (DriverCancellationPenalty) takes a signed amount, so this adapter emits positive for
-- a penalty (DriverFee/wallet debit) and negative for an addition (wallet credit; the
-- legacy DriverFee path cannot pay out and skips with a log).
driverMoneyDeduction :: DCCM.CancellationConsequenceMatrix -> HighPrecMoney -> Maybe HighPrecMoney
driverMoneyDeduction row estimatedFare =
  row.driverDeduction >>= \case
    DExtra.MoneyDeduction money -> Just (abs (fst (moneyDeductionAmount money estimatedFare)))
    DExtra.MoneyAddition money -> Just (negate (abs (fst (moneyDeductionAmount money estimatedFare))))
    _ -> Nothing

driverRideCreditDeduction :: DCCM.CancellationConsequenceMatrix -> HighPrecMoney -> Maybe HighPrecMoney
driverRideCreditDeduction row estimatedFare =
  row.driverDeduction >>= \case
    DExtra.RideCreditDeduction money -> Just (abs (fst (moneyDeductionAmount money estimatedFare)))
    _ -> Nothing

--------------------------------------------------------------------------------------
-- Global fault-rule name registry, stored as a JSON list in system_configs under
-- 'cancellation_fault_rule_registry' (NOT per city). Dashboard validation joins fault
-- rule outputs and matrix rows against it so a typo cannot silently fall through to
-- wildcard rows.

data FaultRuleRegistryEntry = FaultRuleRegistryEntry
  { name :: Text,
    description :: Text,
    active :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON)

faultRuleRegistryConfigId :: Text
faultRuleRegistryConfigId = "cancellation_fault_rule_registry"

getFaultRuleRegistry :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => m [FaultRuleRegistryEntry]
getFaultRuleRegistry = KSQS.findById faultRuleRegistryConfigId <&> maybe [] (fromMaybe [] . decodeFromText)

isRegisteredFaultRule :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Text -> m Bool
isRegisteredFaultRule ruleName = any (\entry -> entry.active && entry.name == ruleName) <$> getFaultRuleRegistry

-- | Replace the whole registry list (dashboard upsert does read-modify-write).
putFaultRuleRegistry :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => [FaultRuleRegistryEntry] -> m ()
putFaultRuleRegistry entries = do
  let encoded = encodeToText entries
  mbExisting <- KSQS.findById faultRuleRegistryConfigId
  case mbExisting of
    Just _ -> updateOneWithKV [Se.Set BeamSC.configValue encoded] [Se.Is BeamSC.id $ Se.Eq faultRuleRegistryConfigId]
    Nothing -> createWithKV KTSC.SystemConfigs {id = faultRuleRegistryConfigId, configValue = encoded}
