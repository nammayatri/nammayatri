module SharedLogic.RoutePassPricing
  ( PassPurchaseSelection (..),
    RoutePassTerm (..),
    RoutePassFlow,
    resolveRoutePassTerm,
    resolveValidityDays,
    normaliseStopCode,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import Control.Applicative ((<|>))
import Domain.Types.FRFSQuoteCategoryType (FRFSQuoteCategoryType (ADULT))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Pass as DPass
import Domain.Types.PassOverrideConfig
import qualified Domain.Types.Person as DP
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow.Fare as FlowFare
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common
import SharedLogic.FRFSUtils (stageFareBetweenStops, stationCodeForMatching)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Tools.Metrics.BAPMetrics as Metrics

type RoutePassFlow m r c =
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    Metrics.HasBAPMetrics m r,
    HasShortDurationRetryCfg r c,
    HasMasterCloudForwarder r
  )

data PassPurchaseSelection = PassPurchaseSelection
  { sourceStopCode :: Maybe Text,
    destinationStopCode :: Maybe Text,
    tripCount :: Maybe Int,
    validityDays :: Maybe Int,
    roundTrip :: Maybe Bool
  }
  deriving (Generic, Show)

data RoutePassTerm = RoutePassTerm
  { sourceStopCode :: Text,
    destinationStopCode :: Text,
    tripCount :: Int,
    unlimitedTripCount :: Bool,
    validityDays :: Maybe Int,
    serviceTier :: Spec.ServiceTierType,
    referenceFare :: HighPrecMoney,
    perTripAmount :: HighPrecMoney,
    amount :: HighPrecMoney,
    originalAmount :: HighPrecMoney,
    overrideConfig :: OverrideBenefitConfig
  }
  deriving (Generic, Show)

resolveRoutePassTerm ::
  RoutePassFlow m r c =>
  DP.Person ->
  DPass.Pass ->
  PassTicketBenefitConfig ->
  PassPurchaseSelection ->
  m RoutePassTerm
resolveRoutePassTerm person pass config selection = do
  sourceStopCode <- selection.sourceStopCode & fromMaybeM (InvalidRequest "sourceStopCode is required for this pass")
  destinationStopCode <- selection.destinationStopCode & fromMaybeM (InvalidRequest "destinationStopCode is required for this pass")
  when (sourceStopCode == destinationStopCode) $
    throwError (InvalidRequest "sourceStopCode and destinationStopCode must be different")
  when (selection.roundTrip == Just True && config.benefit.allowRoundTrip /= Just True) $
    throwError (InvalidRequest "This pass does not offer round trips")
  tripCount <- resolveTripCount config selection.tripCount
  let unlimited = config.benefit.unlimitedTripCount == Just True
  validityDays <- resolveValidityDays pass selection.validityDays
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing pass.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory pass.vehicleType) DIBC.MULTIMODAL
  void $ normaliseStopCode integratedBPPConfig sourceStopCode
  void $ normaliseStopCode integratedBPPConfig destinationStopCode
  let serviceTier = config.pricing.primaryServiceTier
  referenceFare <- passReferenceFare person pass integratedBPPConfig serviceTier sourceStopCode destinationStopCode
  let perTripAmount = roundToUnits (max 0 (referenceFare - pricingSaving config.pricing referenceFare))
  when (perTripAmount <= 0) $
    throwError (InvalidRequest $ "This pass has no price between " <> sourceStopCode <> " and " <> destinationStopCode)
  let soldBenefit =
        OverrideBenefit
          { percentageSaving = config.benefit.percentageSaving,
            fixedSaving = config.benefit.fixedSaving,
            unlimitedTripCount = config.benefit.unlimitedTripCount,
            maximumTripCount = if unlimited then Nothing else Just tripCount,
            maxTicketQuantityPerOverride = config.benefit.maxTicketQuantityPerOverride,
            enforceStopPair = config.benefit.enforceStopPair,
            allowRoundTrip = Just (selection.roundTrip == Just True),
            allowCheaperStopPairs = config.benefit.allowCheaperStopPairs
          }
  pure
    RoutePassTerm
      { sourceStopCode,
        destinationStopCode,
        tripCount,
        unlimitedTripCount = unlimited,
        validityDays,
        serviceTier,
        referenceFare,
        perTripAmount,
        amount = perTripAmount * fromIntegral tripCount,
        originalAmount = referenceFare * fromIntegral tripCount,
        overrideConfig = OverrideBenefitConfig [PassTicketBenefit PassTicketBenefitConfig {pricing = config.pricing, benefit = soldBenefit}]
      }

resolveTripCount :: MonadFlow m => PassTicketBenefitConfig -> Maybe Int -> m Int
resolveTripCount config mbRequested
  | config.benefit.unlimitedTripCount == Just True =
    config.pricing.unlimitedPricedTripCount & fromMaybeM (InvalidRequest "This pass has no priced trip count configured")
  | config.pricing.variableTripCount == Just True = do
    tripCount <- (mbRequested <|> config.benefit.maximumTripCount) & fromMaybeM (InvalidRequest "tripCount is required for this pass")
    let maxTrips = config.pricing.maximumPurchaseableTripCount
    unless (tripCount >= 1 && tripCount <= maxTrips) $
      throwError (InvalidRequest $ "tripCount must be between 1 and " <> show maxTrips)
    pure tripCount
  | otherwise = do
    fixedTripCount <- config.benefit.maximumTripCount & fromMaybeM (InvalidRequest "This pass has no trip count configured")
    whenJust mbRequested $ \requested ->
      when (requested /= fixedTripCount) $
        throwError (InvalidRequest $ "This pass has a fixed trip count of " <> show fixedTripCount)
    pure fixedTripCount

resolveValidityDays :: MonadFlow m => DPass.Pass -> Maybe Int -> m (Maybe Int)
resolveValidityDays pass mbRequested
  | pass.variableValidityDays == Just True = case mbRequested of
    Nothing -> pure pass.maxValidDays
    Just days -> do
      maxDays <- pass.maxValidDays & fromMaybeM (InvalidRequest "This pass has no maximum validity configured")
      unless (days >= 1 && days <= maxDays) $
        throwError (InvalidRequest $ "validityDays must be between 1 and " <> show maxDays)
      pure (Just days)
  | otherwise = case mbRequested of
    Just days
      | Just days /= pass.maxValidDays ->
        throwError (InvalidRequest "This pass has a fixed validity")
    _ -> pure pass.maxValidDays

pricingSaving :: PassPricingConfig -> HighPrecMoney -> HighPrecMoney
pricingSaving pricing fare
  | Just percentage <- pricing.percentageSaving,
    percentage.enabled == Just True =
    fare * percentage.applicableValue / 100
  | Just fixed <- pricing.fixedSaving,
    fixed.enabled == Just True =
    fixed.applicableValue
  | otherwise = 0

passReferenceFare ::
  RoutePassFlow m r c =>
  DP.Person ->
  DPass.Pass ->
  DIBC.IntegratedBPPConfig ->
  Spec.ServiceTierType ->
  Text ->
  Text ->
  m HighPrecMoney
passReferenceFare person pass integratedBPPConfig serviceTier sourceStopCode destinationStopCode = case pass.vehicleType of
  Spec.BUS -> do
    mbPrice <- stageFareBetweenStops integratedBPPConfig Spec.BUS serviceTier pass.merchantOperatingCityId sourceStopCode destinationStopCode
    price <- mbPrice & fromMaybeM noFare
    pure price.amount
  _ -> do
    let fareRoute =
          CallAPI.FareRoute
            { segments = CallAPI.BasicRouteDetail {routeCode = "", startStopCode = sourceStopCode, endStopCode = destinationStopCode, color = Nothing} :| [],
              mbProviderRouteId = Nothing
            }
    (_, fares) <- FlowFare.getFares person.id pass.merchantId pass.merchantOperatingCityId integratedBPPConfig fareRoute pass.vehicleType (Just serviceTier) Nothing [] [] False True
    let onTier = filter ((== serviceTier) . (.vehicleServiceTier.serviceTierType)) fares
        adultPrice fare = (.price.amount) <$> find ((== ADULT) . (.category)) fare.categories
    listToMaybe (mapMaybe adultPrice (onTier <> fares)) & fromMaybeM noFare
  where
    noFare = InvalidRequest $ "No " <> show serviceTier <> " fare between " <> sourceStopCode <> " and " <> destinationStopCode

normaliseStopCode ::
  (MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r c) =>
  DIBC.IntegratedBPPConfig ->
  Text ->
  m Text
normaliseStopCode integratedBPPConfig stopCode =
  stationCodeForMatching integratedBPPConfig stopCode >>= fromMaybeM (InvalidRequest $ "Unknown stop: " <> stopCode)
