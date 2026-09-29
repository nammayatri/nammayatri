{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fare-adjustment management (dev/docs/fare-adjustments-plan.md). Invariants:
--   * scales hard-capped to +-50%, spikes to <= 24h windows, experiments
--     auto-conclude at <= 60d (validTill stamped at activation when absent);
--   * OneWay + Progressive only: every (tier x area) in scope must resolve to
--     an enabled OneWay fare product with a Progressive policy — validated at
--     write time so a scope that could never fire is rejected, not silent;
--   * at most one non-terminal adjustment may intersect a (tiers x areas)
--     slice — activation rejects ANY overlap (serialized by a per-city Redis
--     lock), keeping measurement uncontaminated;
--   * every mutation clears the city cache; activation/abort affect NEW
--     searches only (already-priced transactions replay their pin / the
--     estimate-cached policy — see SharedLogic.FareAdjustment).
module Domain.Action.Dashboard.Management.PricingAdjustment
  ( getPricingAdjustmentList,
    postPricingAdjustmentCreate,
    postPricingAdjustmentUpdate,
    postPricingAdjustmentStatus,
    postPricingAdjustmentPreview,
    getPricingAdjustmentResults,
  )
where

import qualified API.Types.ProviderPlatform.Management.PricingAdjustment as Common
import Control.Applicative ((<|>))
import qualified "lib-dashboard" Dashboard.Common as DCommon
import Data.List (intersect, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareAdjustment as DFA
import qualified Domain.Types.FareAlertSubscription as DFAS
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Email.Flow as Email
import qualified Email.Types as EmailT
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareAdjustment as SFA
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.CachedQueries.FareAdjustment as CQFA
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.Estimate as CHEst
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FareAdjustment as QFA
import qualified Storage.Queries.FareAlertSubscription as QFAS
import qualified Storage.Queries.FareProductExtra as QFareProductExtra

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  pure (merchant, merchantOpCity)

--------------------------------------------------------------------------------
-- hard caps (server-enforced; the UI mirrors them but cannot exceed them)
--------------------------------------------------------------------------------

maxScaleAbsPct :: Double
maxScaleAbsPct = 50

maxSpikeWindowSeconds :: NominalDiffTime
maxSpikeWindowSeconds = 24 * 3600

maxExperimentLifetimeSeconds :: NominalDiffTime
maxExperimentLifetimeSeconds = 60 * 86400

--------------------------------------------------------------------------------
-- CRUD
--------------------------------------------------------------------------------

getPricingAdjustmentList :: ShortId DM.Merchant -> Context.City -> Flow Common.PricingAdjustmentListRes
getPricingAdjustmentList merchantShortId opCity = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  adjustments <- QFA.findAllByMerchantOperatingCityId merchantOpCity.id
  now <- getCurrentTime
  pure $ Common.PricingAdjustmentListRes {adjustments = map (toApiAdjustment now) (sortOn (Down . (.createdAt)) adjustments)}

postPricingAdjustmentCreate :: ShortId DM.Merchant -> Context.City -> Common.PricingAdjustmentReq -> Flow Common.PricingAdjustmentRes
postPricingAdjustmentCreate merchantShortId opCity req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  validateAdjustmentReq merchantOpCity req
  createdBy <- fromMaybeM (InvalidRequest "createdBy missing (must be set by the dashboard proxy)") req.createdBy
  newId <- generateGUID
  now <- getCurrentTime
  QFA.create
    DFA.FareAdjustment
      { id = newId,
        merchantId = merchant.id,
        merchantOperatingCityId = merchantOpCity.id,
        vehicleServiceTiers = req.vehicleServiceTiers,
        areas = req.areas,
        mode = fromApiMode req.mode,
        status = DFA.DRAFT,
        baseFareScalePct = req.baseFareScalePct,
        perKmRateScalePct = req.perKmRateScalePct,
        perMinRateScalePct = req.perMinRateScalePct,
        congestionScalePct = req.congestionScalePct,
        rolloutPercentage = req.rolloutPercentage,
        validFrom = req.validFrom,
        validTill = req.validTill,
        reason = req.reason,
        createdBy,
        createdAt = now,
        updatedAt = now
      }
  CQFA.clearCache merchantOpCity.id
  pure Common.PricingAdjustmentRes {adjustmentId = cast newId, status = Common.DRAFT}

postPricingAdjustmentUpdate :: ShortId DM.Merchant -> Context.City -> Id DCommon.FareAdjustment -> Common.PricingAdjustmentReq -> Flow APISuccess
postPricingAdjustmentUpdate merchantShortId opCity reqAdjustmentId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  adjustment <- findScopedAdjustment merchantOpCity (cast reqAdjustmentId)
  unless (adjustment.status == DFA.DRAFT) $
    throwError (InvalidRequest "only DRAFT adjustments are editable; end it and create a new one instead")
  validateAdjustmentReq merchantOpCity req
  now <- getCurrentTime
  QFA.updateByPrimaryKey
    adjustment
      { DFA.vehicleServiceTiers = req.vehicleServiceTiers,
        DFA.areas = req.areas,
        DFA.mode = fromApiMode req.mode,
        DFA.baseFareScalePct = req.baseFareScalePct,
        DFA.perKmRateScalePct = req.perKmRateScalePct,
        DFA.perMinRateScalePct = req.perMinRateScalePct,
        DFA.congestionScalePct = req.congestionScalePct,
        DFA.rolloutPercentage = req.rolloutPercentage,
        DFA.validFrom = req.validFrom,
        DFA.validTill = req.validTill,
        DFA.reason = req.reason,
        DFA.updatedAt = now
      }
  CQFA.clearCache merchantOpCity.id
  pure Success

postPricingAdjustmentStatus :: ShortId DM.Merchant -> Context.City -> Id DCommon.FareAdjustment -> Common.PricingAdjustmentStatusReq -> Flow APISuccess
postPricingAdjustmentStatus merchantShortId opCity reqAdjustmentId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  -- serialize transitions per city: concurrent activations could otherwise
  -- interleave the overlap scan and the status writes and leave two live
  -- adjustments intersecting the same slice
  Redis.withWaitOnLockRedisWithExpiry (adjustmentStatusLockKey merchantOpCity.id) 10 60 $ do
    adjustment <- findScopedAdjustment merchantOpCity (cast reqAdjustmentId)
    now <- getCurrentTime
    let newStatus = fromApiStatus req.status
    case (adjustment.status, newStatus) of
      (DFA.DRAFT, DFA.ACTIVE) -> do
        activated <- validateActivation merchantOpCity adjustment now
        QFA.updateByPrimaryKey (activated {DFA.status = DFA.ACTIVE, DFA.updatedAt = now} :: DFA.FareAdjustment)
      (DFA.DRAFT, DFA.ENDED) -> QFA.updateStatusById DFA.ENDED adjustment.id
      (DFA.ACTIVE, DFA.ENDED) -> QFA.updateStatusById DFA.ENDED adjustment.id
      (from, to) -> throwError (InvalidRequest $ "transition " <> show from <> " -> " <> show to <> " is not allowed")
    CQFA.clearCache merchantOpCity.id
    logInfo $ "FARE_ADJUSTMENT_STATUS_CHANGE: adjustment " <> adjustment.id.getId <> " -> " <> show newStatus <> " (city " <> merchantOpCity.id.getId <> ")"
    sendAdjustmentAlert merchantOpCity adjustment newStatus
  pure Success

adjustmentStatusLockKey :: Id DMOC.MerchantOperatingCity -> Text
adjustmentStatusLockKey cityId = "FareAdjustment:Status:CityId-" <> cityId.getId

-- | Activation-time checks; returns the adjustment with the experiment
-- auto-conclude deadline stamped when it was absent.
validateActivation :: DMOC.MerchantOperatingCity -> DFA.FareAdjustment -> UTCTime -> Flow DFA.FareAdjustment
validateActivation merchantOpCity adjustment now = do
  -- caps re-checked on the STORED row: a row written before a cap tightening
  -- must not slip through activation
  validateScales adjustment.baseFareScalePct adjustment.perKmRateScalePct adjustment.perMinRateScalePct adjustment.congestionScalePct
  stamped <- case adjustment.mode of
    DFA.SPIKE -> do
      validFrom <- fromMaybeM (InvalidRequest "a spike needs validFrom") adjustment.validFrom
      validTill <- fromMaybeM (InvalidRequest "a spike needs validTill") adjustment.validTill
      when (validTill <= now) $ throwError (InvalidRequest "the spike window is entirely in the past")
      when (diffUTCTime validTill validFrom > maxSpikeWindowSeconds) $
        throwError (InvalidRequest "spike window exceeds the 24h cap")
      pure adjustment
    DFA.EXPERIMENT -> do
      let deadline = addUTCTime maxExperimentLifetimeSeconds now
      case adjustment.validTill of
        Nothing -> pure (adjustment {DFA.validTill = Just deadline} :: DFA.FareAdjustment)
        Just till -> do
          when (till <= now) $ throwError (InvalidRequest "validTill is in the past")
          when (till > deadline) $ throwError (InvalidRequest "experiments auto-conclude within 60 days; validTill exceeds that")
          pure adjustment
  -- overlap: no other live adjustment may intersect this slice (ACTIVE-only
  -- fetch; isLiveAt still filters out elapsed-window rows)
  siblings <- QFA.findAllByCityAndStatus merchantOpCity.id DFA.ACTIVE
  let overlapping =
        [ other
          | other <- siblings,
            other.id /= adjustment.id,
            SFA.isLiveAt now other,
            not (null (other.vehicleServiceTiers `intersect` adjustment.vehicleServiceTiers)),
            areasIntersect other.areas adjustment.areas
        ]
  whenJust (listToMaybe overlapping) $ \other ->
    throwError (InvalidRequest $ "activation rejected: adjustment " <> other.id.getId <> " (" <> other.reason <> ") is live on an intersecting tier/area slice")
  pure stamped
  where
    -- Nothing = all areas, so it intersects everything
    areasIntersect Nothing _ = True
    areasIntersect _ Nothing = True
    areasIntersect (Just as) (Just bs) = not (null (as `intersect` bs))

--------------------------------------------------------------------------------
-- validation
--------------------------------------------------------------------------------

validateScales :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Flow ()
validateScales baseFareScalePct perKmRateScalePct perMinRateScalePct congestionScalePct = do
  let scales = catMaybes [baseFareScalePct, perKmRateScalePct, perMinRateScalePct, congestionScalePct]
  when (null scales) $ throwError (InvalidRequest "at least one scale is required")
  forM_ scales $ \pct ->
    when (abs pct > maxScaleAbsPct) $
      throwError (InvalidRequest $ "scale " <> show pct <> "% exceeds the hard cap of +-" <> show maxScaleAbsPct <> "%")

validateAdjustmentReq :: DMOC.MerchantOperatingCity -> Common.PricingAdjustmentReq -> Flow ()
validateAdjustmentReq merchantOpCity req = do
  when (null req.vehicleServiceTiers) $ throwError (InvalidRequest "at least one service tier is required")
  when (T.null (T.strip req.reason)) $ throwError (InvalidRequest "a reason is required")
  validateScales req.baseFareScalePct req.perKmRateScalePct req.perMinRateScalePct req.congestionScalePct
  whenJust req.areas $ \as -> when (null as) $ throwError (InvalidRequest "areas must be non-empty when present (absent = all areas)")
  case req.mode of
    Common.SPIKE -> do
      validFrom <- fromMaybeM (InvalidRequest "a spike needs validFrom") req.validFrom
      validTill <- fromMaybeM (InvalidRequest "a spike needs validTill") req.validTill
      when (validTill <= validFrom) $ throwError (InvalidRequest "validTill must be after validFrom")
      when (diffUTCTime validTill validFrom > maxSpikeWindowSeconds) $
        throwError (InvalidRequest "spike window exceeds the 24h cap")
      whenJust req.rolloutPercentage $ \_ -> throwError (InvalidRequest "rolloutPercentage is for experiments; a spike always applies to everyone in scope")
    Common.EXPERIMENT -> do
      pct <- fromMaybeM (InvalidRequest "an experiment needs rolloutPercentage") req.rolloutPercentage
      unless (pct >= 1 && pct <= 99) $ throwError (InvalidRequest "rolloutPercentage must be within 1..99 (100% is just the new price — edit the rate card instead)")
      whenJust ((,) <$> req.validFrom <*> req.validTill) $ \(from, till) ->
        when (till <= from) $ throwError (InvalidRequest "validTill must be after validFrom")
  -- dead-scope guard: every (tier x area) must resolve to an enabled OneWay
  -- fare product with a Progressive policy, else part of the scope could
  -- never fire (or would silently degrade to stamp-only at runtime)
  products <- QFareProductExtra.findAllFareProductByMerchantOpCityIdAllStates merchantOpCity.id
  let scopeAreas = fromMaybe [SL.Default] req.areas
  forM_ req.vehicleServiceTiers $ \tier ->
    forM_ scopeAreas $ \area -> do
      let candidates = [p | p <- products, p.enabled, p.vehicleServiceTier == tier, p.area == area, isPlainOneWay p.tripCategory]
      product' <-
        fromMaybeM (InvalidRequest $ "no enabled OneWay fare product for tier " <> show tier <> " / area " <> show area <> " — the adjustment would never fire there") $
          listToMaybe candidates
      farePolicy <- CQFP.findById Nothing product'.farePolicyId >>= fromMaybeM (InvalidRequest $ "fare policy missing for tier " <> show tier <> " / area " <> show area)
      unless (DFP.getFarePolicyType farePolicy == DFP.Progressive) $
        throwError (InvalidRequest $ "tier " <> show tier <> " / area " <> show area <> " uses a " <> show (DFP.getFarePolicyType farePolicy) <> " policy; adjustments support Progressive only (v1)")

isPlainOneWay :: DTC.TripCategory -> Bool
isPlainOneWay = \case
  DTC.OneWay v -> v /= DTC.MeterRide
  _ -> False

findScopedAdjustment :: DMOC.MerchantOperatingCity -> Id DFA.FareAdjustment -> Flow DFA.FareAdjustment
findScopedAdjustment merchantOpCity adjustmentId = do
  adjustment <- QFA.findByPrimaryKey adjustmentId >>= fromMaybeM (InvalidRequest $ "Fare adjustment not found: " <> adjustmentId.getId)
  unless (adjustment.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Fare adjustment belongs to a different operating city")
  pure adjustment

--------------------------------------------------------------------------------
-- preview: before/after headline numbers off the tier's Default OneWay policy
--------------------------------------------------------------------------------

postPricingAdjustmentPreview :: ShortId DM.Merchant -> Context.City -> Common.PricingAdjustmentPreviewReq -> Flow Common.PricingAdjustmentPreviewRes
postPricingAdjustmentPreview merchantShortId opCity req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  validateScales req.baseFareScalePct req.perKmRateScalePct req.perMinRateScalePct req.congestionScalePct
  products <- QFareProductExtra.findAllFareProductByMerchantOpCityIdAllStates merchantOpCity.id
  let candidates = [p | p <- products, p.enabled, p.vehicleServiceTier == req.vehicleServiceTier, p.area == SL.Default, isPlainOneWay p.tripCategory]
  product' <-
    fromMaybeM (InvalidRequest $ "no enabled OneWay fare product for tier " <> show req.vehicleServiceTier <> " in the Default area") $
      listToMaybe candidates
  farePolicy <- CQFP.findById Nothing product'.farePolicyId >>= fromMaybeM (InvalidRequest "fare policy missing for the tier's Default OneWay product")
  details <- case farePolicy.farePolicyDetails of
    DFP.ProgressiveDetails d -> pure d
    _ -> throwError (InvalidRequest "the tier's Default OneWay policy is not Progressive; adjustments support Progressive only (v1)")
  let firstKmSection = NE.head (NE.sortWith (.startDistance) details.perExtraKmRateSections)
      firstMinSection = NE.head . NE.sortWith (.rideDurationInMin) <$> details.perMinRateSections
      staticMultiplier = DFP.congestionChargeMultiplierToCentesimal <$> farePolicy.congestionChargeMultiplier
      before =
        Common.PricingAdjustmentSnapshot
          { baseFare = details.baseFare,
            firstPerKmRate = firstKmSection.perExtraKmRate,
            firstPerMinRate = (.perMinRate.amount) <$> firstMinSection,
            congestionMultiplier = staticMultiplier
          }
      after =
        Common.PricingAdjustmentSnapshot
          { baseFare = SFA.scaleMoney req.baseFareScalePct details.baseFare,
            firstPerKmRate = SFA.scaleMoney req.perKmRateScalePct firstKmSection.perExtraKmRate,
            firstPerMinRate = SFA.scaleMoney req.perMinRateScalePct . (.perMinRate.amount) <$> firstMinSection,
            congestionMultiplier =
              if isJust req.congestionScalePct
                then Just (SFA.scaleCentesimal req.congestionScalePct (fromMaybe 1.0 staticMultiplier))
                else staticMultiplier
          }
  pure Common.PricingAdjustmentPreviewRes {before, after}

--------------------------------------------------------------------------------
-- results: arm-vs-arm over the adjustment's live window
--------------------------------------------------------------------------------

getPricingAdjustmentResults :: ShortId DM.Merchant -> Context.City -> Id DCommon.FareAdjustment -> Flow Common.PricingAdjustmentResultsRes
getPricingAdjustmentResults merchantShortId opCity reqAdjustmentId = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  adjustment <- findScopedAdjustment merchantOpCity (cast reqAdjustmentId)
  now <- getCurrentTime
  let windowFrom = fromMaybe adjustment.createdAt (adjustment.validFrom <|> Just adjustment.createdAt)
      windowTill = maybe now (min now) adjustment.validTill
  stats <- CHEst.pricingAdjustmentComparison merchantOpCity.id adjustment.id.getId windowFrom windowTill
  pure
    Common.PricingAdjustmentResultsRes
      { windowFrom,
        windowTill,
        rows =
          [ Common.PricingAdjustmentArmStat {serviceTier = tier, arm, estimates = total, avgMultiplier = avgM, avgMaxFare = avgFare}
            | (tier, arm, total, avgM, avgFare) <- stats
          ]
      }

--------------------------------------------------------------------------------
-- API <-> domain mapping
--------------------------------------------------------------------------------

toApiAdjustment :: UTCTime -> DFA.FareAdjustment -> Common.PricingAdjustment
toApiAdjustment now adjustment =
  Common.PricingAdjustment
    { adjustmentId = cast adjustment.id,
      vehicleServiceTiers = adjustment.vehicleServiceTiers,
      areas = adjustment.areas,
      mode = toApiMode adjustment.mode,
      status = toApiStatus now adjustment,
      baseFareScalePct = adjustment.baseFareScalePct,
      perKmRateScalePct = adjustment.perKmRateScalePct,
      perMinRateScalePct = adjustment.perMinRateScalePct,
      congestionScalePct = adjustment.congestionScalePct,
      rolloutPercentage = adjustment.rolloutPercentage,
      validFrom = adjustment.validFrom,
      validTill = adjustment.validTill,
      reason = adjustment.reason,
      createdBy = adjustment.createdBy,
      createdAt = adjustment.createdAt
    }

-- | An ACTIVE row whose window elapsed reads as EXPIRED: expiry is an
-- evaluation-time fact, not a status write, so the DB keeps ACTIVE and the
-- API synthesizes the display status.
toApiStatus :: UTCTime -> DFA.FareAdjustment -> Common.PricingAdjustmentStatus
toApiStatus now adjustment = case adjustment.status of
  DFA.DRAFT -> Common.DRAFT
  DFA.ACTIVE -> if maybe False (<= now) adjustment.validTill then Common.EXPIRED else Common.ACTIVE
  DFA.ENDED -> Common.ENDED
  DFA.EXPIRED -> Common.EXPIRED

fromApiStatus :: Common.PricingAdjustmentStatus -> DFA.FareAdjustmentStatus
fromApiStatus = \case
  Common.DRAFT -> DFA.DRAFT
  Common.ACTIVE -> DFA.ACTIVE
  Common.ENDED -> DFA.ENDED
  Common.EXPIRED -> DFA.EXPIRED

toApiMode :: DFA.FareAdjustmentMode -> Common.PricingAdjustmentMode
toApiMode = \case
  DFA.EXPERIMENT -> Common.EXPERIMENT
  DFA.SPIKE -> Common.SPIKE

fromApiMode :: Common.PricingAdjustmentMode -> DFA.FareAdjustmentMode
fromApiMode = \case
  Common.EXPERIMENT -> DFA.EXPERIMENT
  Common.SPIKE -> DFA.SPIKE

--------------------------------------------------------------------------------
-- lifecycle alert emails (existing FareAlertSubscription rails)
--------------------------------------------------------------------------------

sendAdjustmentAlert :: DMOC.MerchantOperatingCity -> DFA.FareAdjustment -> DFA.FareAdjustmentStatus -> Flow ()
sendAdjustmentAlert merchantOpCity adjustment newStatus =
  fork "fare adjustment alert email" $ do
    subscriptions <- QFAS.findAllByMerchantOperatingCityId merchantOpCity.id
    let recipients = [s.email | s <- subscriptions, s.alertType == DFAS.ADJUSTMENTS]
    if null recipients
      then logInfo $ "fare adjustment " <> adjustment.id.getId <> " -> " <> show newStatus <> " but no ADJUSTMENTS subscribers in " <> show merchantOpCity.city
      else do
        transporterConfig <- getTransporterConfig' merchantOpCity.id
        emailServiceConfig <- asks (.emailServiceConfig)
        let fromEmail = fromMaybe "no-reply@nammayatri.in" transporterConfig.tdsFromEmail
            modeLabel = case adjustment.mode of
              DFA.EXPERIMENT -> "Experiment"
              DFA.SPIKE -> "Spike"
            scaleLine label = maybe "" (\pct -> "  - " <> label <> ": " <> T.pack (show pct) <> "%\n")
            subject = "[Fare " <> modeLabel <> "] " <> show newStatus <> " in " <> T.pack (show merchantOpCity.city)
            body =
              modeLabel <> " \"" <> adjustment.reason <> "\" is now " <> show newStatus
                <> " in "
                <> T.pack (show merchantOpCity.city)
                <> ".\n\nTiers: "
                <> T.intercalate ", " (map (T.pack . show) adjustment.vehicleServiceTiers)
                <> "\nAreas: "
                <> maybe "all" (T.intercalate ", " . map (T.pack . show)) adjustment.areas
                <> "\nScales:\n"
                <> scaleLine "base fare" adjustment.baseFareScalePct
                <> scaleLine "per-km rate" adjustment.perKmRateScalePct
                <> scaleLine "per-min rate" adjustment.perMinRateScalePct
                <> scaleLine "congestion" adjustment.congestionScalePct
                <> maybe "" (\pct -> "Rollout: " <> T.pack (show pct) <> "% of riders\n") adjustment.rolloutPercentage
                <> maybe "" (\till -> "Valid till: " <> T.pack (show till) <> "\n") adjustment.validTill
                <> "Created by: "
                <> adjustment.createdBy
                <> "\n\nThis is a system-generated email from the pricing dashboard."
        liftIO $ Email.sendPlainEmail emailServiceConfig fromEmail recipients subject body EmailT.Text

getTransporterConfig' :: Id DMOC.MerchantOperatingCity -> Flow TransporterConfig
getTransporterConfig' mocId =
  getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
    >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
