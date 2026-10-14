-- | Pure tests for the fleet tag blocks (SharedLogic.FleetTags): what joining / leaving a
-- fleet does to the driver's tags and vehicle tiers, the vehicle-rebuild guard and the
-- auto-accept carry-over, and the reserved tag names of the driver tag API.
module FleetTagsSpec (spec) where

import qualified Dashboard.Common as DC
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian)
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVV
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.FleetTags
import Test.Hspec
import qualified Prelude as P

now :: UTCTime
now = UTCTime (fromGregorian 2026 10 14) 0

tag :: Text -> LYT.TagNameValueExpiry
tag = LYT.TagNameValueExpiry

-- | Expiry in the namma-tag format, relative to 'now'.
expiringTag :: Text -> NominalDiffTime -> LYT.TagNameValueExpiry
expiringTag nameValue offset = tag (nameValue <> "#" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (addUTCTime offset now)))

decodeOrFail :: A.FromJSON a => A.Value -> a
decodeOrFail value = case A.fromJSON value of
  A.Success a -> a
  A.Error err -> P.error err

baseVehicle :: DV.Vehicle
baseVehicle =
  decodeOrFail $
    A.object
      [ "color" A..= ("white" :: Text),
        "driverId" A..= ("driver-1" :: Text),
        "merchantId" A..= ("merchant-1" :: Text),
        "model" A..= ("model" :: Text),
        "registrationNo" A..= ("KA01AB1234" :: Text),
        "selectedServiceTiers" A..= ([] :: [DTC.ServiceTierType]),
        "variant" A..= DVV.SEDAN,
        "vehicleClass" A..= ("4WN" :: Text),
        "createdAt" A..= now,
        "updatedAt" A..= now
      ]

vehicleWith :: DVV.VehicleVariant -> [DTC.ServiceTierType] -> Maybe [DTC.ServiceTierType] -> DV.Vehicle
vehicleWith variant selected autoAccept =
  baseVehicle {DV.variant = variant, DV.selectedServiceTiers = selected, DV.selectedAutoAcceptTiers = autoAccept}

baseTier :: DVST.VehicleServiceTier
baseTier =
  decodeOrFail $
    A.object
      [ "allowedVehicleVariant" A..= ([] :: [DVV.VehicleVariant]),
        "autoSelectedVehicleVariant" A..= ([] :: [DVV.VehicleVariant]),
        "defaultForVehicleVariant" A..= ([] :: [DVV.VehicleVariant]),
        "id" A..= ("tier-1" :: Text),
        "merchantId" A..= ("merchant-1" :: Text),
        "merchantOperatingCityId" A..= ("city-1" :: Text),
        "name" A..= ("tier" :: Text),
        "priority" A..= (0 :: Int),
        "serviceTierType" A..= DTC.SEDAN,
        "specialZoneQueueCalloutVariants" A..= ([] :: [DVV.VehicleVariant]),
        "createdAt" A..= now,
        "updatedAt" A..= now
      ]

-- | A tier with auto-acceptance enabled in this mode, servable by these variants.
autoAcceptTier :: DTC.ServiceTierType -> DC.AutoAcceptanceMode -> [DVV.VehicleVariant] -> DVST.VehicleServiceTier
autoAcceptTier tierType mode variants =
  baseTier
    { DVST.serviceTierType = tierType,
      DVST.allowedVehicleVariant = variants,
      DVST.autoAcceptanceConfig = Just DC.AutoAcceptanceConfig {minWalletBalance = Nothing, mode = mode, enabled = True}
    }

cohortGated :: DVST.VehicleServiceTier -> DVST.VehicleServiceTier
cohortGated tier = tier {DVST.availabilityCheckConfig = Just DC.AvailabilityCheckConfig {radiusMeters = 1000, minDriverCount = 1}}

-- City: SEDAN (AutoAcceptOptional), COMFY (AutoAcceptOnly, sedans), SUV (AutoAcceptOnly, SUVs only).
cityTiers :: [DVST.VehicleServiceTier]
cityTiers =
  [ autoAcceptTier DTC.SEDAN DC.AutoAcceptOptional [DVV.SEDAN],
    autoAcceptTier DTC.COMFY DC.AutoAcceptOnly [DVV.SEDAN],
    autoAcceptTier DTC.SUV DC.AutoAcceptOnly [DVV.SUV]
  ]

cityTierTypes :: [DTC.ServiceTierType]
cityTierTypes = map (.serviceTierType) cityTiers

bothOn, autoAssignOnly, ignoreOnly, bothOff :: FleetTagSettings
bothOn = FleetTagSettings {autoAssignDefaultOn = True, ignoreDriverAreaAndDistancePreference = True}
autoAssignOnly = FleetTagSettings {autoAssignDefaultOn = True, ignoreDriverAreaAndDistancePreference = False}
ignoreOnly = FleetTagSettings {autoAssignDefaultOn = False, ignoreDriverAreaAndDistancePreference = True}
bothOff = FleetTagSettings {autoAssignDefaultOn = False, ignoreDriverAreaAndDistancePreference = False}

autoAssignTiersOf :: [LYT.TagNameValueExpiry] -> [Text]
autoAssignTiersOf = autoAssignTagValues now

spec :: Spec
spec = do
  describe "fleetTagsAfterJoin" $ do
    it "writes a never-expiring AutoAssign tag with every city auto-accept tier" $ do
      let tags = fleetTagsAfterJoin now autoAssignOnly cityTierTypes []
      tags `shouldBe` [tag "AutoAssign#SEDAN&COMFY&SUV"]

    it "merges with the existing AutoAssign values and keeps other tags" $ do
      let existing = [tag "PetDriver#true", tag "AutoAssign#BIKE&SEDAN"]
          tags = fleetTagsAfterJoin now autoAssignOnly cityTierTypes existing
      autoAssignTiersOf tags `shouldBe` ["BIKE", "SEDAN", "COMFY", "SUV"]
      tags `shouldContain` [tag "PetDriver#true"]
      length (filter (T.isPrefixOf "AutoAssign#" . (.getTagNameValueExpiry)) tags) `shouldBe` 1

    it "replaces an expiring AutoAssign tag by a never-expiring one, dropping expired values" $ do
      let existing = [expiringTag "AutoAssign#BIKE" (-3600)]
      fleetTagsAfterJoin now autoAssignOnly [DTC.SEDAN] existing `shouldBe` [tag "AutoAssign#SEDAN"]

    it "writes AreaPreferenceEnabled#false and DistancePreferenceEnabled#false when preferences are ignored" $ do
      let tags = fleetTagsAfterJoin now ignoreOnly cityTierTypes [tag "AreaPreferenceEnabled#true"]
      tags `shouldMatchList` [tag "AreaPreferenceEnabled#false", tag "DistancePreferenceEnabled#false"]

    it "writes both blocks when both switches are on" $ do
      let tags = fleetTagsAfterJoin now bothOn [DTC.SEDAN] []
      tags `shouldMatchList` [tag "AutoAssign#SEDAN", tag "AreaPreferenceEnabled#false", tag "DistancePreferenceEnabled#false"]

    it "leaves the driver unchanged when both switches are off" $ do
      let existing = [tag "AutoAssign#BIKE", tag "AreaPreferenceEnabled#true"]
      fleetTagsAfterJoin now bothOff cityTierTypes existing `shouldBe` existing

    it "leaves the tags unchanged when the city has no auto-accept tier" $
      fleetTagsAfterJoin now autoAssignOnly [] [tag "PetDriver#true"] `shouldBe` [tag "PetDriver#true"]

    it "is idempotent" $ do
      let existing = [tag "PetDriver#true", tag "AutoAssign#BIKE"]
          once = fleetTagsAfterJoin now bothOn cityTierTypes existing
      fleetTagsAfterJoin now bothOn cityTierTypes once `shouldBe` once

  describe "fleetTagsAfterLeave" $ do
    it "removes AutoAssign and DistancePreferenceEnabled, keeping other tags" $ do
      let existing = [tag "PetDriver#true", tag "AutoAssign#SEDAN", tag "DistancePreferenceEnabled#false"]
      fleetTagsAfterLeave False existing `shouldBe` [tag "PetDriver#true"]

    it "removes AreaPreferenceEnabled#false when the left fleet ignored preferences" $
      fleetTagsAfterLeave True [tag "AreaPreferenceEnabled#false", tag "AutoAssign#SEDAN"] `shouldBe` []

    it "keeps the driver's own AreaPreferenceEnabled#false when the left fleet did not ignore preferences" $
      fleetTagsAfterLeave False [tag "AreaPreferenceEnabled#false"] `shouldBe` [tag "AreaPreferenceEnabled#false"]

    it "keeps AreaPreferenceEnabled#true" $
      fleetTagsAfterLeave True [tag "AreaPreferenceEnabled#true"] `shouldBe` [tag "AreaPreferenceEnabled#true"]

    it "undoes fleetTagsAfterJoin for a driver with no fleet tags" $ do
      let existing = [tag "PetDriver#true"]
      fleetTagsAfterLeave True (fleetTagsAfterJoin now bothOn cityTierTypes existing) `shouldBe` existing

    it "is idempotent" $ do
      let once = fleetTagsAfterLeave True [tag "AutoAssign#SEDAN", tag "AreaPreferenceEnabled#false", tag "PetDriver#true"]
      fleetTagsAfterLeave True once `shouldBe` once

  describe "autoAssignVehicleFields (VEHICLE PART)" $ do
    it "puts every city auto-accept tier into selectedAutoAcceptTiers" $ do
      let vehicle = autoAssignVehicleFields now Nothing cityTiers (vehicleWith DVV.SEDAN [DTC.SEDAN] Nothing)
      vehicle.selectedAutoAcceptTiers `shouldBe` Just cityTierTypes

    it "adds only the AutoAcceptOnly tiers the vehicle serves to selectedServiceTiers" $ do
      let vehicle = autoAssignVehicleFields now Nothing cityTiers (vehicleWith DVV.SEDAN [DTC.SEDAN] Nothing)
      vehicle.selectedServiceTiers `shouldBe` [DTC.SEDAN, DTC.COMFY]

    it "never adds a tier the vehicle cannot serve" $ do
      let vehicle = autoAssignVehicleFields now Nothing cityTiers (vehicleWith DVV.SUV [] Nothing)
      vehicle.selectedServiceTiers `shouldBe` [DTC.SUV]

    it "does not add a cohort-gated AutoAcceptOnly tier without the cohort tag" $ do
      let tiers = [cohortGated (autoAcceptTier DTC.COMFY DC.AutoAcceptOnly [DVV.SEDAN])]
      (autoAssignVehicleFields now Nothing tiers (vehicleWith DVV.SEDAN [] Nothing)).selectedServiceTiers `shouldBe` []
      (autoAssignVehicleFields now (Just [tag "Cohort#COMFY"]) tiers (vehicleWith DVV.SEDAN [] Nothing)).selectedServiceTiers `shouldBe` [DTC.COMFY]

    it "merges with existing selections and is idempotent" $ do
      let vehicle = vehicleWith DVV.SEDAN [DTC.ECO] (Just [DTC.BIKE])
          once = autoAssignVehicleFields now Nothing cityTiers vehicle
      once.selectedAutoAcceptTiers `shouldBe` Just (DTC.BIKE : cityTierTypes)
      once.selectedServiceTiers `shouldBe` [DTC.ECO, DTC.COMFY]
      let twice = autoAssignVehicleFields now Nothing cityTiers once
      (twice.selectedServiceTiers, twice.selectedAutoAcceptTiers) `shouldBe` (once.selectedServiceTiers, once.selectedAutoAcceptTiers)

  describe "clearAutoAssignVehicleFields" $ do
    it "clears selectedAutoAcceptTiers and drops AutoAcceptOnly tiers" $ do
      let vehicle = clearAutoAssignVehicleFields [DTC.COMFY, DTC.SUV] (vehicleWith DVV.SEDAN [DTC.SEDAN, DTC.COMFY] (Just [DTC.SEDAN, DTC.COMFY]))
      vehicle.selectedAutoAcceptTiers `shouldBe` Just []
      vehicle.selectedServiceTiers `shouldBe` [DTC.SEDAN]

    it "undoes the VEHICLE PART for a vehicle without auto-accept" $ do
      let vehicle = vehicleWith DVV.SEDAN [DTC.SEDAN] Nothing
          cleared = clearAutoAssignVehicleFields [DTC.COMFY, DTC.SUV] (autoAssignVehicleFields now Nothing cityTiers vehicle)
      cleared.selectedServiceTiers `shouldBe` vehicle.selectedServiceTiers
      cleared.selectedAutoAcceptTiers `shouldBe` Just []

  describe "shouldApplyFleetVehiclePart (vehicle rebuilt from an RC)" $ do
    it "applies to a first vehicle under a fleet with autoAssignDefaultOn" $
      shouldApplyFleetVehiclePart True Nothing `shouldBe` True

    it "applies when the previous vehicle had auto-accept on" $
      shouldApplyFleetVehiclePart True (Just (vehicleWith DVV.SEDAN [] (Just [DTC.SEDAN]))) `shouldBe` True

    it "skips when the previous vehicle had auto-accept off (owner's per-driver OFF survives)" $ do
      shouldApplyFleetVehiclePart True (Just (vehicleWith DVV.SEDAN [] (Just []))) `shouldBe` False
      shouldApplyFleetVehiclePart True (Just (vehicleWith DVV.SEDAN [] Nothing)) `shouldBe` False

    it "never applies when the fleet has autoAssignDefaultOn off" $
      shouldApplyFleetVehiclePart False Nothing `shouldBe` False

  describe "carryOverAutoAcceptTiers" $ do
    let rebuilt = vehicleWith DVV.SEDAN [DTC.SEDAN] Nothing
        carryCityTiers =
          [ autoAcceptTier DTC.SEDAN DC.AutoAcceptOptional [DVV.SEDAN],
            autoAcceptTier DTC.COMFY DC.AutoAcceptOptional [DVV.SEDAN],
            autoAcceptTier DTC.SUV DC.AutoAcceptOptional [DVV.SUV],
            autoAcceptTier DTC.BIKE DC.AutoAcceptOnly [DVV.BIKE]
          ]
    it "carries the auto-accept tiers and their base tiers when the variant is unchanged" $ do
      let vehicle = carryOverAutoAcceptTiers carryCityTiers (Just (vehicleWith DVV.SEDAN [DTC.SEDAN, DTC.COMFY] (Just [DTC.SEDAN, DTC.COMFY]))) rebuilt
      vehicle.selectedAutoAcceptTiers `shouldBe` Just [DTC.SEDAN, DTC.COMFY]
      vehicle.selectedServiceTiers `shouldBe` [DTC.SEDAN, DTC.COMFY]

    it "never selects a carried tier the vehicle cannot serve (all-city auto-accept list)" $ do
      let vehicle = carryOverAutoAcceptTiers carryCityTiers (Just (vehicleWith DVV.SEDAN [DTC.SEDAN] (Just [DTC.SEDAN, DTC.COMFY, DTC.SUV, DTC.BIKE]))) rebuilt
      vehicle.selectedAutoAcceptTiers `shouldBe` Just [DTC.SEDAN, DTC.COMFY, DTC.SUV, DTC.BIKE]
      vehicle.selectedServiceTiers `shouldBe` [DTC.SEDAN, DTC.COMFY]

    it "carries nothing when the variant changed" $ do
      let vehicle = carryOverAutoAcceptTiers carryCityTiers (Just (vehicleWith DVV.SUV [DTC.SUV] (Just [DTC.SUV]))) rebuilt
      (vehicle.selectedServiceTiers, vehicle.selectedAutoAcceptTiers) `shouldBe` ([DTC.SEDAN], Nothing)

    it "carries nothing when the previous vehicle had auto-accept off or there was none" $ do
      let offVehicle = carryOverAutoAcceptTiers carryCityTiers (Just (vehicleWith DVV.SEDAN [DTC.SEDAN] (Just []))) rebuilt
          noVehicle = carryOverAutoAcceptTiers carryCityTiers Nothing rebuilt
      (offVehicle.selectedServiceTiers, offVehicle.selectedAutoAcceptTiers) `shouldBe` ([DTC.SEDAN], Nothing)
      (noVehicle.selectedServiceTiers, noVehicle.selectedAutoAcceptTiers) `shouldBe` ([DTC.SEDAN], Nothing)

  describe "isReservedSystemTagName (driver tag API)" $ do
    it "reserves the fleet and preference tags, case-insensitively and trimmed" $
      map isReservedSystemTagName ["AutoAssign", " autoassign ", "DistancePreferenceEnabled", "AreaPreferenceEnabled", "AreaPreferenceRadius", "AreaPreference_tdr1"]
        `shouldBe` replicate 6 True

    it "does not reserve other tags" $
      map isReservedSystemTagName ["PetDriver", "Cohort", "AutoAssignX", "Area"] `shouldBe` [False, False, False, False]

  describe "tag helpers" $ do
    it "applyIgnorePreferenceTags False removes both preference switches" $
      applyIgnorePreferenceTags False [tag "AreaPreferenceEnabled#false", tag "DistancePreferenceEnabled#false", tag "PetDriver#true"]
        `shouldBe` [tag "PetDriver#true"]

    it "clearDistancePreferenceOffTag drops only DistancePreferenceEnabled" $
      clearDistancePreferenceOffTag [tag "DistancePreferenceEnabled#false", tag "AreaPreferenceEnabled#false"]
        `shouldBe` [tag "AreaPreferenceEnabled#false"]

    it "isAreaPreferenceEnabledTagOn is False only for an explicit AreaPreferenceEnabled#false" $ do
      isAreaPreferenceEnabledTagOn [] `shouldBe` True
      isAreaPreferenceEnabledTagOn [tag "AreaPreferenceEnabled#true"] `shouldBe` True
      isAreaPreferenceEnabledTagOn [tag "AreaPreferenceEnabled#false"] `shouldBe` False

    it "autoAssignTagValues ignores expired AutoAssign tags" $ do
      autoAssignTagValues now [expiringTag "AutoAssign#SEDAN&COMFY" 3600] `shouldBe` ["SEDAN", "COMFY"]
      autoAssignTagValues now [expiringTag "AutoAssign#SEDAN" (-3600)] `shouldBe` []
