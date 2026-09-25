{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.DriverCoins.IncentiveOverlay
  ( incentiveTargetCompletedOverlayKey,
    isRideCompletionIncentiveEvent,
    sendIncentiveTargetCompletedOverlay,
  )
where

import qualified Dashboard.Common as DashboardCommon
import qualified Dashboard.Common.DriverCoins as DCT
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisLTSFlowEnv)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import Storage.Beam.SpecialZone ()
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)

incentiveTargetCompletedOverlayKey :: Text
incentiveTargetCompletedOverlayKey = "INCENTIVE_TARGET_COMPLETED"

overlayTemplateText :: Text -> Text
overlayTemplateText txt = "{#" <> txt <> "#}"

isRideCompletionIncentiveEvent :: DCT.DriverCoinsFunctionType -> Bool
isRideCompletionIncentiveEvent = \case
  DCT.RidesCompleted _ -> True
  DCT.RidesCompletedOnServiceTier _ _ -> True
  DCT.RidesCompletedInSpecialLocation _ _ -> True
  DCT.DriverIncentiveCohortRidesCompleted _ -> True
  DCT.DriverIncentiveCohortRidesCompletedSlot _ _ -> True
  _ -> False

buildIncentiveTargetDescription ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DCT.DriverCoinsFunctionType ->
  m Text
buildIncentiveTargetDescription merchantOpCityId = \case
  DCT.RidesCompleted n ->
    pure $ formatRidesCompleted n Nothing
  DCT.RidesCompletedOnServiceTier tier n -> do
    tierLabel <- resolveServiceTierDisplayName merchantOpCityId tier
    pure $ formatRidesCompleted n (Just tierLabel)
  DCT.RidesCompletedInSpecialLocation area n -> do
    areaLabel <- specialLocationAreaLabel area
    pure $ formatRidesCompleted n (Just areaLabel)
  DCT.DriverIncentiveCohortRidesCompleted n ->
    pure $ formatRidesCompleted n Nothing <> " in your incentive window"
  DCT.DriverIncentiveCohortRidesCompletedSlot slot n ->
    pure $ formatRidesCompleted n Nothing <> " in the " <> slot <> " incentive slot"
  other ->
    pure $ show other

formatRidesCompleted :: Int -> Maybe Text -> Text
formatRidesCompleted n mbQualifier =
  let rideWord = if n == 1 then "ride" else "rides"
   in case mbQualifier of
        Nothing -> show n <> " " <> rideWord <> " completed"
        Just qualifier -> show n <> " " <> qualifier <> " " <> rideWord <> " completed"

resolveSpecialLocationName ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id SL.SpecialLocation ->
  m Text
resolveSpecialLocationName slId = do
  mbSpecialLocation <- QSpecialLocation.findById slId
  pure $ maybe slId.getId (.locationName) mbSpecialLocation

specialLocationAreaLabel ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SL.Area ->
  m Text
specialLocationAreaLabel = \case
  SL.Default -> pure "special location"
  SL.Pickup slId _ -> resolveSpecialLocationName slId
  SL.Drop slId -> resolveSpecialLocationName slId
  SL.PickupDrop pickupId dropId _ -> do
    pickupName <- resolveSpecialLocationName pickupId
    dropName <- resolveSpecialLocationName dropId
    pure $ pickupName <> " to " <> dropName

resolveServiceTierDisplayName ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DashboardCommon.ServiceTierType ->
  m Text
resolveServiceTierDisplayName merchantOpCityId tier = do
  mbVehicleServiceTier <- CQVST.findByServiceTierTypeAndCityId tier merchantOpCityId Nothing
  pure $ maybe (formatServiceTierType tier) (.name) mbVehicleServiceTier

formatServiceTierType :: DashboardCommon.ServiceTierType -> Text
formatServiceTierType tier =
  T.intercalate " " $
    map capitalizeWord $
      T.splitOn "_" $
        T.toLower $
          T.pack $
            show tier
  where
    capitalizeWord word =
      case T.uncons word of
        Nothing -> word
        Just (firstChar, rest) -> T.cons (Char.toUpper firstChar) rest

applyIncentiveOverlayTemplates :: Text -> Int -> Text -> Text
applyIncentiveOverlayTemplates incentiveTarget coinsAwarded txt =
  T.replace (overlayTemplateText "incentiveTarget") incentiveTarget
    . T.replace (overlayTemplateText "coinsAwarded") (show coinsAwarded)
    $ txt

sendIncentiveTargetCompletedOverlay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HedisLTSFlowEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DCT.DriverCoinsFunctionType ->
  Int ->
  m ()
sendIncentiveTargetCompletedOverlay _ _ _ 0 = pure ()
sendIncentiveTargetCompletedOverlay merchantOpCityId driverId eventFunction coinsAwarded
  | not (isRideCompletionIncentiveEvent eventFunction) = pure ()
  | otherwise = do
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    mOverlay <-
      CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
        merchantOpCityId
        incentiveTargetCompletedOverlayKey
        (fromMaybe ENGLISH driver.language)
        Nothing
        Nothing
        Nothing
    whenJust mOverlay $ \overlay -> do
      incentiveTarget <- buildIncentiveTargetDescription merchantOpCityId eventFunction
      let applyTemplates = applyIncentiveOverlayTemplates incentiveTarget coinsAwarded
          overlay' =
            overlay
              { DOverlay.title = fmap applyTemplates overlay.title,
                DOverlay.description = fmap applyTemplates overlay.description,
                DOverlay.okButtonText = fmap applyTemplates overlay.okButtonText,
                DOverlay.cancelButtonText = fmap applyTemplates overlay.cancelButtonText,
                DOverlay.toastMessage = fmap applyTemplates overlay.toastMessage,
                DOverlay.actions = [incentiveTargetCompletedOverlayKey]
              }
      sendOverlay merchantOpCityId driver $ mkOverlayReq overlay'
