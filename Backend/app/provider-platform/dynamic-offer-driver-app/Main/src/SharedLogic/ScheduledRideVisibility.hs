{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Staged visibility of unassigned scheduled rides on the driver board.
-- The stage for a ride is chosen from the time left to pickup, and every rule present in that stage must pass.
-- A fact that could not be loaded makes the rule that needs it fail, except that a driver with no rating passes the rating rule.
module SharedLogic.ScheduledRideVisibility
  ( DriverVisibilityFacts (..),
    BookingVisibilityFacts (..),
    VisibilityFactNeeds (..),
    resolveVisibilityStage,
    factsNeededBy,
    isVisibleUnderRules,
    parseVisibilityStagesJson,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, sortOn)
import qualified Data.Text as T
import Domain.Types.Common (ServiceTierType)
import Domain.Types.TransporterConfig (ScheduledRideVisibilityRules, ScheduledRideVisibilityStage)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

data DriverVisibilityFacts = DriverVisibilityFacts
  { driverRating :: Maybe Double,
    driverCompletedRides :: Maybe Int,
    driverCancellationRatePct :: Maybe Double,
    driverActiveTags :: [Text],
    driverLocation :: Maybe LatLong
  }

data BookingVisibilityFacts = BookingVisibilityFacts
  { bookingPickup :: LatLong,
    walletMeetsMinimum :: Maybe Bool,
    isFavouriteOfRider :: Maybe Bool
  }

-- | Which facts a rule set needs, so that only those are loaded.
data VisibilityFactNeeds = VisibilityFactNeeds
  { needsDriverStats :: Bool,
    needsWalletBalance :: Bool,
    needsFavouriteLookup :: Bool
  }

-- | The applicable stage with the largest threshold that is still below the remaining time.
-- A ride closer to pickup than every threshold matches no stage and is open to everyone.
-- Tier-specific stages win over a generic stage at the same threshold.
resolveVisibilityStage :: ServiceTierType -> [ScheduledRideVisibilityStage] -> NominalDiffTime -> Maybe ScheduledRideVisibilityStage
resolveVisibilityStage serviceTier stages remaining =
  listToMaybe $
    sortOn (\s -> (negate s.untilRemainingMinutes, isNothing s.vehicleServiceTiers)) $
      filter
        ( \s ->
            maybe True (serviceTier `elem`) s.vehicleServiceTiers
              && s.untilRemainingMinutes >= 0
              && fromIntegral (s.untilRemainingMinutes * 60) < remaining
        )
        stages

factsNeededBy :: ScheduledRideVisibilityRules -> VisibilityFactNeeds
factsNeededBy rules =
  VisibilityFactNeeds
    { needsDriverStats = any isJust [void rules.minRating, void rules.maxCancellationRatePct, void rules.minCompletedRides],
      needsWalletBalance = rules.requiresWalletBalance == Just True,
      needsFavouriteLookup = rules.allowFavouriteDriversOnly == Just True
    }

isVisibleUnderRules :: DriverVisibilityFacts -> BookingVisibilityFacts -> ScheduledRideVisibilityRules -> Bool
isVisibleUnderRules driverFacts bookingFacts rules =
  and
    [ walletRulePasses,
      tagRulePasses,
      favouriteRulePasses,
      ratingRulePasses,
      atMost rules.maxCancellationRatePct driverFacts.driverCancellationRatePct,
      atLeast rules.minCompletedRides driverFacts.driverCompletedRides,
      nearbyRulePasses
    ]
  where
    atLeast :: Ord a => Maybe a -> Maybe a -> Bool
    atLeast Nothing _ = True
    atLeast (Just required) actual = maybe False (>= required) actual

    atMost :: Ord a => Maybe a -> Maybe a -> Bool
    atMost Nothing _ = True
    atMost (Just allowed) actual = maybe False (<= allowed) actual

    ratingRulePasses = maybe True (\required -> maybe True (>= required) driverFacts.driverRating) rules.minRating
    walletRulePasses = rules.requiresWalletBalance /= Just True || bookingFacts.walletMeetsMinimum == Just True
    favouriteRulePasses = rules.allowFavouriteDriversOnly /= Just True || bookingFacts.isFavouriteOfRider == Just True

    tagRulePasses = all (tagIsActive driverFacts.driverActiveTags) (fromMaybe [] rules.requiredTags)

    nearbyRulePasses
      | rules.requiresNearby /= Just True = True
      | otherwise = case (rules.pickupRadiusMeters, driverFacts.driverLocation) of
        (Just radius, Just location) -> distanceBetweenInMeters location bookingFacts.bookingPickup <= fromIntegral radius
        _ -> False

-- | Tags are "name#value" with an optional value, and a driver value may hold several values joined by "&".
tagIsActive :: [Text] -> Text -> Bool
tagIsActive activeTags required = any matchesActive activeTags
  where
    (requiredName, requiredValue) = splitTag required
    matchesActive active =
      let (activeName, activeValue) = splitTag active
       in activeName == requiredName && maybe True (\value -> maybe False (elem value . T.splitOn "&") activeValue) requiredValue

splitTag :: Text -> (Text, Maybe Text)
splitTag tag =
  let (name, rest) = T.breakOn "#" tag
      value = T.drop 1 rest
   in (name, if T.null value then Nothing else Just value)

knownStageKeys :: [Text]
knownStageKeys = ["stage", "untilRemainingMinutes", "vehicleServiceTiers", "rules"]

knownRuleKeys :: [Text]
knownRuleKeys =
  [ "requiresWalletBalance",
    "requiredTags",
    "allowFavouriteDriversOnly",
    "minRating",
    "maxCancellationRatePct",
    "minCompletedRides",
    "requiresNearby",
    "pickupRadiusMeters"
  ]

-- | Parses and validates the stages sent from the dashboard. Unknown keys are rejected here because the
-- stored-config decoder ignores them, which would silently leave a stage more open than intended.
parseVisibilityStagesJson :: A.Value -> Either Text [ScheduledRideVisibilityStage]
parseVisibilityStagesJson value = do
  stageValues <- case value of
    A.Array stageArray -> Right (toList stageArray)
    _ -> Left "scheduledRideVisibilityStages must be a list"
  forM_ (zip [1 :: Int ..] stageValues) $ \(index, stageValue) -> checkStageKeys index stageValue
  stages <- traverse decodeStage stageValues
  validateVisibilityStages stages
  pure stages
  where
    decodeStage stageValue = case A.fromJSON stageValue of
      A.Success decoded -> Right decoded
      A.Error err -> Left $ "scheduledRideVisibilityStages is malformed: " <> T.pack err

    checkStageKeys index stageValue = case stageValue of
      A.Object stageObj -> do
        rejectUnknown ("stage " <> T.pack (show index)) knownStageKeys stageObj
        case KM.lookup "rules" stageObj of
          Just (A.Object rulesObj) -> rejectUnknown ("rules of stage " <> T.pack (show index)) knownRuleKeys rulesObj
          _ -> Left $ "rules of stage " <> T.pack (show index) <> " must be an object"
      _ -> Left $ "stage " <> T.pack (show index) <> " must be an object"

    rejectUnknown label allowed obj =
      case filter (`notElem` allowed) (AK.toText <$> KM.keys obj) of
        [] -> Right ()
        unknown -> Left $ "Unknown key(s) in " <> label <> ": " <> T.intercalate ", " unknown

validateVisibilityStages :: [ScheduledRideVisibilityStage] -> Either Text ()
validateVisibilityStages stages = do
  let thresholds = (.untilRemainingMinutes) <$> stages
  when (any (< 0) thresholds) $ Left "untilRemainingMinutes cannot be negative"
  forM_ stages $ \s -> do
    let label = "stage " <> s.stage <> ": "
        rules = s.rules
    whenJust s.vehicleServiceTiers $ \tiers -> do
      when (null tiers) $ Left $ label <> "vehicleServiceTiers cannot be empty"
      when (length (nub tiers) /= length tiers) $ Left $ label <> "vehicleServiceTiers cannot contain duplicates"
    whenJust rules.minRating $ \r -> unless (r >= 0 && r <= 5) $ Left $ label <> "minRating must be between 0 and 5"
    whenJust rules.maxCancellationRatePct $ \r -> unless (r >= 0 && r <= 100) $ Left $ label <> "maxCancellationRatePct must be between 0 and 100"
    whenJust rules.minCompletedRides $ \n -> when (n < 0) $ Left $ label <> "minCompletedRides cannot be negative"
    whenJust rules.pickupRadiusMeters $ \n -> when (n <= 0) $ Left $ label <> "pickupRadiusMeters must be positive"
    when (rules.requiresNearby == Just True && isNothing rules.pickupRadiusMeters) $ Left $ label <> "requiresNearby needs pickupRadiusMeters"
    whenJust rules.requiredTags $ \tags -> do
      when (any (T.null . T.strip) tags) $ Left $ label <> "requiredTags cannot contain an empty tag"
      when (length (nub tags) /= length tags) $ Left $ label <> "requiredTags cannot contain duplicates"
