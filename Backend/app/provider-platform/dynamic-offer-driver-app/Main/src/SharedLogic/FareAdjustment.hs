{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fare adjustments (dev/docs/fare-adjustments-plan.md): scale-only overlays
-- on the RESOLVED fare policy. Arm decisions are made ONCE per search
-- transaction (deterministic salted hash of the customer phone for
-- experiments; everyone for spikes) and pinned in Redis; every later
-- evaluation of the same transaction — select re-resolution, end-ride
-- cache-miss — replays the pin, so activation/abort/expiry only ever affect
-- NEW searches. The estimate/quote-cached FullFarePolicy already carries the
-- applied scales, so the pin is the fallback path, not the primary one.
module SharedLogic.FareAdjustment
  ( FareAdjustmentArm (..),
    PinnedFareAdjustment (..),
    armText,
    mkAdjustmentDpVersion,
    isLiveAt,
    experimentArmBucket,
    decideAndPinFareAdjustments,
    resolvePinnedFareAdjustments,
    resolveMatchingAdjustment,
    adjustmentTargetsCongestion,
    applyFareAdjustmentToPolicy,
    scaleMoney,
    scaleCentesimal,
  )
where

import qualified Crypto.Hash as Hash
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.Common as DTC
import Domain.Types.FareAdjustment
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import Numeric (readHex)
import qualified SharedLogic.FarePolicy.Conversions as FarePolicyD
import qualified Storage.CachedQueries.FareAdjustment as CQFA
import qualified Storage.Queries.FareAdjustment as QFA

data FareAdjustmentArm = Treatment | Control
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | One pin entry per adjustment that was live for the city when the search
-- was priced. Control entries are recorded too: control estimates must be
-- stamped with the adjustment id so arm-vs-arm comparison has its control
-- population.
data PinnedFareAdjustment = PinnedFareAdjustment
  { adjustmentId :: Text,
    arm :: FareAdjustmentArm
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

armText :: FareAdjustmentArm -> Text
armText Treatment = "treatment"
armText Control = "control"

mkAdjustmentDpVersion :: FareAdjustment -> Text
mkAdjustmentDpVersion adjustment = "FareAdjustment:" <> adjustment.id.getId

-- | ACTIVE and inside its validity window. The window check at evaluation
-- time IS the auto-expiry: an elapsed spike stops applying on the next new
-- search without any status write.
isLiveAt :: UTCTime -> FareAdjustment -> Bool
isLiveAt now adjustment =
  adjustment.status == ACTIVE
    && maybe True (<= now) adjustment.validFrom
    && maybe True (now <) adjustment.validTill

-- | Deterministic bucket in [0, 99]: SHA256 of (adjustmentId : phone). Salting
-- with the adjustment id makes every experiment's rider split independent —
-- the same riders are not the permanent test group for everything.
experimentArmBucket :: Id FareAdjustment -> Text -> Int
experimentArmBucket adjustmentId phone =
  let digest = Hash.hashWith Hash.SHA256 (TE.encodeUtf8 (adjustmentId.getId <> ":" <> phone))
   in case readHex (take 8 (show digest)) of
        [(n :: Integer, _)] -> fromIntegral (n `mod` 100)
        _ -> 0 -- unreachable: a SHA256 digest always shows as hex

decideArm :: UTCTime -> Maybe Text -> FareAdjustment -> Maybe PinnedFareAdjustment
decideArm now mbPhone adjustment
  | not (isLiveAt now adjustment) = Nothing
  | otherwise = case adjustment.mode of
    SPIKE -> Just $ PinnedFareAdjustment adjustment.id.getId Treatment
    EXPERIMENT ->
      let arm = case (mbPhone, adjustment.rolloutPercentage) of
            -- no phone (dashboard-created searches etc.): control — never
            -- randomly, so the estimate stream stays deterministic
            (Just phone, Just pct) | experimentArmBucket adjustment.id phone < pct -> Treatment
            _ -> Control
       in Just $ PinnedFareAdjustment adjustment.id.getId arm

-- | Called once from the Beckn search handler BEFORE fare policies resolve.
-- Writes nothing when no adjustment is live — the common case costs one
-- cached-list read.
decideAndPinFareAdjustments ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Text ->
  m ()
decideAndPinFareAdjustments merchantOpCityId mbPhone transactionId = do
  adjustments <- CQFA.findActiveByMerchantOperatingCityId merchantOpCityId
  now <- getCurrentTime
  let decisions = mapMaybe (decideArm now mbPhone) adjustments
  unless (null decisions) $ do
    Hedis.withCrossAppRedis $ Hedis.setExp (fareAdjustmentPinKey transactionId) decisions fareAdjustmentPinTtlSeconds
    logInfo $ "FARE_ADJUSTMENT_PIN: txn " <> transactionId <> " decisions " <> show ((\d -> (d.adjustmentId, armText d.arm)) <$> decisions)

resolvePinnedFareAdjustments :: (CacheFlow m r) => Maybe Text -> m [PinnedFareAdjustment]
resolvePinnedFareAdjustments = \case
  Nothing -> pure []
  Just ctxId -> fromMaybe [] <$> Hedis.withCrossAppRedis (Hedis.safeGet (fareAdjustmentPinKey ctxId))

fareAdjustmentPinKey :: Text -> Text
fareAdjustmentPinKey ctxId = "driver-offer:FareAdjustment:Pin:" <> ctxId

-- | Must outlive the longest search -> end-ride span, same as the surge pin:
-- expiry means an end-ride cache-miss recompute silently loses the scales the
-- search was priced with.
fareAdjustmentPinTtlSeconds :: Int
fareAdjustmentPinTtlSeconds = 30 * 86400

-- | The pinned adjustment governing this fare product, if any. Pinned ids are
-- resolved against the small ACTIVE city cache; a pinned row that has since
-- gone terminal (ended/expired) falls out of that cache but must keep
-- replaying for the pin's lifetime, so it is re-fetched by primary key — a
-- bounded lookup, never a scan of the city's adjustment history. Activation
-- rejects slice overlap, so at most one pin entry can match a given
-- (tier, area) — the first match is THE match.
resolveMatchingAdjustment ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  [PinnedFareAdjustment] ->
  ServiceTierType ->
  DTC.TripCategory ->
  SL.Area ->
  m (Maybe (FareAdjustment, FareAdjustmentArm))
resolveMatchingAdjustment _ [] _ _ _ = pure Nothing
resolveMatchingAdjustment merchantOpCityId pins serviceTier tripCategory area = do
  activeAdjustments <- CQFA.findActiveByMerchantOperatingCityId merchantOpCityId
  resolved <- forM pins $ \pin ->
    case find (\a -> a.id.getId == pin.adjustmentId) activeAdjustments of
      Just adjustment -> pure (Just (adjustment, pin.arm))
      Nothing -> fmap (\adjustment -> (adjustment, pin.arm)) <$> QFA.findByPrimaryKey (Id pin.adjustmentId)
  pure $ listToMaybe [(adjustment, arm) | Just (adjustment, arm) <- resolved, scopeMatches adjustment]
  where
    scopeMatches adjustment =
      serviceTier `elem` adjustment.vehicleServiceTiers
        && maybe True (area `elem`) adjustment.areas
        && isPlainOneWay tripCategory
    isPlainOneWay = \case
      DTC.OneWay v -> v /= DTC.MeterRide
      _ -> False

adjustmentTargetsCongestion :: FareAdjustment -> Bool
adjustmentTargetsCongestion adjustment = isJust adjustment.congestionScalePct

-- | Treatment: scale the progressive fields and (when targeted) replace the
-- congestion outcome. The adjustment's id + arm are NOT set here — they flow
-- in via CongestionChargeDetails at policy construction, for both arms, so
-- control estimates carry the stamp too. Applies to Progressive OneWay
-- policies only — the dashboard write path validates that every tier x area
-- in scope resolves to one, so a runtime mismatch is a rare race (policy type
-- changed after activation) and degrades to stamp-only.
applyFareAdjustmentToPolicy :: (Log m, Monad m) => Maybe (FareAdjustment, FareAdjustmentArm) -> FarePolicyD.FullFarePolicy -> m FarePolicyD.FullFarePolicy
applyFareAdjustmentToPolicy Nothing fullFarePolicy = pure fullFarePolicy
applyFareAdjustmentToPolicy (Just (adjustment, arm)) fullFarePolicy =
  case (arm, fullFarePolicy.farePolicyDetails) of
    (Control, _) -> pure fullFarePolicy
    (Treatment, FarePolicyD.ProgressiveDetails details) -> do
      let scaledDetails = scaleProgressiveDetails adjustment details
          withCongestion =
            if adjustmentTargetsCongestion adjustment
              then
                fullFarePolicy
                  { FarePolicyD.congestionChargeMultiplier =
                      Just $ FarePolicyD.BaseFareAndExtraDistanceFare $ scaleCentesimal adjustment.congestionScalePct (staticMultiplier fullFarePolicy.congestionChargeMultiplier),
                    FarePolicyD.dpVersion = Just (mkAdjustmentDpVersion adjustment)
                  }
              else fullFarePolicy
      pure (withCongestion {FarePolicyD.farePolicyDetails = FarePolicyD.ProgressiveDetails scaledDetails} :: FarePolicyD.FullFarePolicy)
    (Treatment, _) -> do
      logWarning $ "FARE_ADJUSTMENT_SCOPE_MISMATCH: adjustment " <> adjustment.id.getId <> " matched a non-Progressive policy " <> fullFarePolicy.id.getId <> "; stamping only"
      pure fullFarePolicy
  where
    staticMultiplier = \case
      Just m -> FarePolicyD.congestionChargeMultiplierToCentesimal m
      Nothing -> 1.0

scaleProgressiveDetails :: FareAdjustment -> FarePolicyD.FPProgressiveDetails -> FarePolicyD.FPProgressiveDetails
scaleProgressiveDetails adjustment details =
  details
    { FarePolicyD.baseFare = scaleMoney adjustment.baseFareScalePct details.baseFare,
      FarePolicyD.perExtraKmRateSections =
        NE.map (\section -> section {FarePolicyD.perExtraKmRate = scaleMoney adjustment.perKmRateScalePct section.perExtraKmRate} :: FarePolicyD.FPProgressiveDetailsPerExtraKmRateSection) details.perExtraKmRateSections,
      FarePolicyD.perMinRateSections =
        (NE.map (\section -> section {FarePolicyD.perMinRate = modifyPrice section.perMinRate (scaleMoney adjustment.perMinRateScalePct)} :: FarePolicyD.FPProgressiveDetailsPerMinRateSection)) <$> details.perMinRateSections
    }

scaleMoney :: Maybe Double -> HighPrecMoney -> HighPrecMoney
scaleMoney Nothing money = money
scaleMoney (Just pct) (HighPrecMoney money) = HighPrecMoney (money * toRational (1 + pct / 100))

scaleCentesimal :: Maybe Double -> Centesimal -> Centesimal
scaleCentesimal Nothing multiplier = multiplier
scaleCentesimal (Just pct) multiplier = multiplier * realToFrac (1 + pct / 100)
