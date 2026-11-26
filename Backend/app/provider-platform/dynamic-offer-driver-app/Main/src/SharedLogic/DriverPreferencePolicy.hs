{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet preference locks (fleet_config.preference_policy), v1: VALIDATION ONLY.
--
-- A DRIVER request that moves a locked preference away from its allowed value is rejected at the top of the driver
-- handler, before any write. Nothing is written or forced; ops, dashboard, system and fleet paths are not validated.
--
-- How to add a lockable preference:
--
--   1. Add a constructor to 'DDPP.DriverPreference' (Domain.Types.DriverPreferencePolicy). Stored rows use the
--      normal JSON decode; a row that does not decode reads as no policy.
--   2. Say which 'DDPP.PreferenceValue' kind it takes in 'valueKindMatches'.
--   3. Guard every DRIVER write path of it with a @validate*@ helper below. Never guard shared or ops writers.
--   4. If a driver tag carries it, guard POST /ui/driver/tag/update too ('validateTagWrite').
--   5. If the app should grey the control out, expose a lock flag on the driver GET it reads.
--
-- AreaPreference and TripDistance can only be hidden in v1; their locks come later with the ops area-preference
-- endpoint.
module SharedLogic.DriverPreferencePolicy
  ( module SharedLogic.DriverPreferencePolicy.Context,
    lockFor,
    isLocked,
    isHidden,
    validatePolicyConfig,
    validateToggle,
    validateAirConditioned,
    TierChangeRequest (..),
    protectedTiers,
    validateServiceTierChanges,
    validateLegacyTierRebuild,
    validateTagWrite,
  )
where

import qualified Data.List as DL
import qualified Data.Text as T
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPreferencePolicy as DDPP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.DriverPreferencePolicy.Context
import qualified SharedLogic.FleetDriverSettings as FDS
import Tools.Error

-------------------------------------------------------------------------------
-- Lookup: 'SharedLogic.DriverPreferencePolicy.Context' (re-exported here)

lockFor :: DDPP.DriverPreference -> DDPP.PreferencePolicy -> Maybe DDPP.PreferenceLock
lockFor preference policy = find ((== preference) . (.preference)) policy.locked

isLocked :: DDPP.DriverPreference -> DDPP.PreferencePolicy -> Bool
isLocked preference = isJust . lockFor preference

isHidden :: DDPP.DriverPreference -> DDPP.PreferencePolicy -> Bool
isHidden preference policy = preference `elem` policy.hidden

throwLocked :: (MonadThrow m, Log m) => DDPP.DriverPreference -> m a
throwLocked preference = do
  logInfo $ "Rejecting driver change to fleet-locked preference " <> show preference
  throwError $ case preference of
    DDPP.ServiceTier -> ServiceTierLockedByFleet
    _ -> PreferenceLockedByFleet

-------------------------------------------------------------------------------
-- Config save (dashboard)

-- | Which value kind each preference takes. A null value (frozen) is allowed for any lockable preference.
valueKindMatches :: DDPP.DriverPreference -> DDPP.PreferenceValue -> Bool
valueKindMatches preference = \case
  DDPP.AutoAssignTiers -> preference == DDPP.ServiceTier
  DDPP.ToggleValue _ -> preference `elem` [DDPP.AirConditioned, DDPP.IntraCity, DDPP.InterCity, DDPP.Rental]

-- | Rejects a policy the fleet config update must not store. 'hidden' takes any preference.
validatePolicyConfig :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> m ()
validatePolicyConfig policy = do
  let preferences = map (.preference) policy.locked
      duplicates = DL.nub [p | p <- preferences, length (filter (== p) preferences) > 1]
  unless (null duplicates) $
    throwError $ InvalidPreferencePolicy $ "Duplicate locked preferences: " <> T.intercalate ", " (map show duplicates)
  forM_ policy.locked $ \lock -> do
    when (lock.preference `elem` [DDPP.AreaPreference, DDPP.TripDistance]) $
      throwError $ InvalidPreferencePolicy "AreaPreference / TripDistance can only be hidden for now"
    whenJust lock.value $ \value ->
      unless (valueKindMatches lock.preference value) $
        throwError $ InvalidPreferencePolicy $ show lock.preference <> " takes a " <> expectedKind lock.preference <> " value (or null)"
  where
    expectedKind :: DDPP.DriverPreference -> Text
    expectedKind = \case
      DDPP.ServiceTier -> "AutoAssignTiers"
      _ -> "ToggleValue"

-------------------------------------------------------------------------------
-- Driver request validation

-- | Toggle rule. With @ToggleValue v@: a change is allowed only toward @v@. Frozen (null, or a value of the wrong
-- kind): no change. A field the driver didn't send (Nothing) is never a change.
validateToggle :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> DDPP.DriverPreference -> Bool -> Maybe Bool -> m ()
validateToggle policy preference stored mbRequested =
  whenJust (lockFor preference policy) $ \lock -> whenJust mbRequested $ \requested ->
    when (requested /= stored && Just requested /= lockedToggle lock) $ throwLocked preference
  where
    lockedToggle lock = case lock.value of
      Just (DDPP.ToggleValue v) -> Just v
      _ -> Nothing

-- | AC: the toggle rule, except that with @ToggleValue@ the driver may still turn AC off while an AC usage restriction
-- from rider complaints is on them.
validateAirConditioned :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> DI.DriverInformation -> Bool -> Maybe Bool -> m ()
validateAirConditioned policy driverInfo stored mbRequested =
  unless (turningOffUnderRestriction && hasToggleValue) $
    validateToggle policy DDPP.AirConditioned stored mbRequested
  where
    turningOffUnderRestriction = mbRequested == Just False && driverInfo.acUsageRestrictionType /= DI.NoRestriction
    hasToggleValue = case lockFor DDPP.AirConditioned policy >>= (.value) of
      Just (DDPP.ToggleValue _) -> True
      _ -> False

-- | One tier as the driver asks for it: base tier selected, and auto-accept (Nothing = not sent).
data TierChangeRequest = TierChangeRequest
  { tier :: ServiceTierType,
    isSelected :: Bool,
    autoAccept :: Maybe Bool
  }

-- | Tiers a ServiceTier lock protects for this driver: on the driver's AutoAssign tag, served by the driver's vehicle
-- and not usage-restricted. Takes 'SharedLogic.VehicleServiceTier.fetchVehicleTierForDriverWithUsageRestriction'
-- output. A driver without the tag has nothing protected.
protectedTiers :: UTCTime -> [LYT.TagNameValueExpiry] -> [(DVST.VehicleServiceTier, Bool)] -> [ServiceTierType]
protectedTiers now driverTags driverTiers =
  [ tier.serviceTierType
    | (tier, isUsageRestricted) <- driverTiers,
      not isUsageRestricted,
      Yudhishthira.elemTagValue (LYT.TagName FDS.autoAssignTagName) (show tier.serviceTierType) liveTags
  ]
  where
    liveTags = Yudhishthira.filterExpiredTags' now driverTags

-- | ServiceTier lock. With @AutoAssignTiers@: a protected tier that is selected can't be turned off, and its
-- auto-accept can't be turned off. Frozen: no change at all to a protected tier's selection or auto-accept.
validateServiceTierChanges :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> [ServiceTierType] -> DV.Vehicle -> [TierChangeRequest] -> m ()
validateServiceTierChanges policy protected vehicle requests =
  whenJust (lockFor DDPP.ServiceTier policy) $ \lock ->
    forM_ (filter ((`elem` protected) . (.tier)) requests) $ \request -> do
      let selectedNow = request.tier `elem` vehicle.selectedServiceTiers
          autoAcceptNow = request.tier `elem` fromMaybe [] vehicle.selectedAutoAcceptTiers
          frozen = lock.value /= Just DDPP.AutoAssignTiers
          turnsOff = (selectedNow && not request.isSelected) || (autoAcceptNow && request.autoAccept == Just False)
          changes = request.isSelected /= selectedNow || maybe False (/= autoAcceptNow) request.autoAccept
      when (turnsOff || (frozen && changes)) $ throwLocked DDPP.ServiceTier

-- | The legacy profile update rebuilds selectedServiceTiers from the downgrade switches: rejected outright while the
-- fleet protects any tier of the driver.
validateLegacyTierRebuild :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> [ServiceTierType] -> m ()
validateLegacyTierRebuild policy protected =
  when (isLocked DDPP.ServiceTier policy && not (null protected)) $ throwLocked DDPP.ServiceTier

-- | The generic tag endpoint can't write the AutoAssign tag while ServiceTier is locked.
validateTagWrite :: (MonadThrow m, Log m) => DDPP.PreferencePolicy -> Text -> m ()
validateTagWrite policy tagName =
  when (tagName == FDS.autoAssignTagName && isLocked DDPP.ServiceTier policy) $ throwLocked DDPP.ServiceTier
