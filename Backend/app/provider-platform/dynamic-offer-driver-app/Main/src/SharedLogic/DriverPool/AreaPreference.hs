{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver "preferred area" preference: stored entirely as tags on
-- 'person.driverTag' (no dedicated driver_information columns), matched
-- against the ride's DROP location only (no pickup, no weighting).
--
-- Storage shape, following the existing "one value per tag name" convention
-- (same one 'PetDriver#"true"' already uses):
--
--   cells method  -> one tag per selected cell: "AreaPreference_<geohash>#<areaName>"
--   radius method -> one tag: "AreaPreferenceRadius#<lat>&<lon>&<radiusMeters>"
--
-- The two are mutually exclusive; the write path (Domain.Action.UI.DriverAreaPreference)
-- clears the other method's tags before writing the active one.
--
-- Everything here is pure -- no DB, no Redis, no monad -- so it can be exercised
-- directly in a repl/test without standing up the pooling stack.
module SharedLogic.DriverPool.AreaPreference
  ( DriverAreaPreference (..),
    minSelectableCellsDefault,
    areaPreferenceCellTagPrefix,
    areaPreferenceCellTagName,
    areaPreferenceRadiusTagName,
    mkCellTagValue,
    mkRadiusTagValue,
    parseRadiusTagValue,
    driverAreaPreferenceFromTags,
    areaPreferenceTagNamesToClear,
    tagNamesForSelection,
    matchesRadius,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps (LatLong (..))
import Kernel.Types.Common
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Lib.Yudhishthira.Types as LYT

-- | A driver's preferred area, in exactly one of the two supported forms. The two
-- are mutually exclusive by product decision, so this is a sum type rather than a
-- record of optional halves.
data DriverAreaPreference
  = -- | Driver painted a set of geohash cells on the map. Each cell is paired with
    -- its display area name (carried in the tag value, so re-showing it never needs
    -- an extra lookup at read time).
    PreferredGeohashCells [(Text, Maybe Text)]
  | -- | Driver dropped a pin and grew a radius around it.
    PreferredRadius LatLong Meters
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- | Fallback minimum when a city has no configured value. Kept only as a safety
-- net -- the real minimum is meant to come from TransporterConfig.
minSelectableCellsDefault :: Int
minSelectableCellsDefault = 4

-- | Namespace prefix for cell tags, so the write path can find-and-clear exactly
-- this driver's area-preference tags without disturbing any other feature's tags,
-- and so a raw geohash string is never mistaken for an unrelated tag name.
areaPreferenceCellTagPrefix :: Text
areaPreferenceCellTagPrefix = "AreaPreference_"

areaPreferenceCellTagName :: Text -> Text
areaPreferenceCellTagName geohash = areaPreferenceCellTagPrefix <> geohash

areaPreferenceRadiusTagName :: Text
areaPreferenceRadiusTagName = "AreaPreferenceRadius"

-- | Tag value for a cell tag: just the area name (empty when the cell has no
-- mapping row -- the driver picked it, so it must not silently vanish).
mkCellTagValue :: Maybe Text -> Text
mkCellTagValue = fromMaybe ""

-- | Tag value for the radius tag: "lat&lon&radiusMeters", the same '&'-joined
-- multi-value convention 'Lib.Yudhishthira.Tools.Utils.parseTagValueFromText'
-- already parses tag values with.
mkRadiusTagValue :: LatLong -> Meters -> Text
mkRadiusTagValue center radius =
  T.intercalate "&" [showT center.lat, showT center.lon, showT (getMeters radius)]
  where
    showT :: Show a => a -> Text
    showT = T.pack . show

parseRadiusTagValue :: Text -> Maybe (LatLong, Meters)
parseRadiusTagValue value = case T.splitOn "&" value of
  [latText, lonText, radiusText] -> do
    lat <- readDouble latText
    lon <- readDouble lonText
    radius <- readDouble radiusText
    pure (LatLong lat lon, Meters (round radius))
  _ -> Nothing
  where
    readDouble t = case TR.double t of
      Right (d, "") -> Just d
      _ -> Nothing

-- | Reconstruct the active preference (if any) from a driver's raw tag list.
-- The radius tag wins if a row somehow carries both -- that state should be
-- unreachable (the write path clears the other method's tags in the same
-- update), but must degrade to a sane preference rather than silently
-- matching on the wrong method.
driverAreaPreferenceFromTags :: [LYT.TagNameValueExpiry] -> Maybe DriverAreaPreference
driverAreaPreferenceFromTags tags =
  case mapMaybe asRadiusTag tags of
    (pref : _) -> Just pref
    [] -> case mapMaybe asCellTag tags of
      [] -> Nothing
      cells -> Just (PreferredGeohashCells cells)
  where
    asRadiusTag tag = do
      (name, value) <- splitTagNameValue tag
      guard (name == areaPreferenceRadiusTagName)
      (center, radius) <- parseRadiusTagValue value
      pure $ PreferredRadius center radius
    asCellTag tag = do
      (name, value) <- splitTagNameValue tag
      geohash <- T.stripPrefix areaPreferenceCellTagPrefix name
      pure (geohash, if T.null value then Nothing else Just value)

splitTagNameValue :: LYT.TagNameValueExpiry -> Maybe (Text, Text)
splitTagNameValue (LYT.TagNameValueExpiry txt) = case T.splitOn "#" txt of
  (name : value : _) -> Just (name, value)
  [name] -> Just (name, "")
  _ -> Nothing

-- | Every tag name that must be cleared before writing a new selection --
-- both the radius tag and every existing cell tag, regardless of which
-- method is being replaced.
areaPreferenceTagNamesToClear :: [LYT.TagNameValueExpiry] -> [Text]
areaPreferenceTagNamesToClear tags =
  [name | tag <- tags, Just (name, _) <- [splitTagNameValue tag], isAreaPreferenceTagName name]
  where
    isAreaPreferenceTagName name = name == areaPreferenceRadiusTagName || areaPreferenceCellTagPrefix `T.isPrefixOf` name

-- | The tag name/value pairs to write for a given selection.
tagNamesForSelection :: DriverAreaPreference -> [(Text, Text)]
tagNamesForSelection = \case
  PreferredGeohashCells cells -> [(areaPreferenceCellTagName geohash, mkCellTagValue areaName) | (geohash, areaName) <- cells]
  PreferredRadius center radius -> [(areaPreferenceRadiusTagName, mkRadiusTagValue center radius)]

-- | True when the ride's drop point falls inside the driver's chosen radius.
-- Drop only -- pickup is deliberately not considered (product decision: the
-- preference is about where the driver ends up, not where they start).
--
-- Cell-method matching has no equivalent function here: at match time it's a
-- direct tag-name lookup against the driver's raw tag blob (does a tag named
-- 'areaPreferenceCellTagName dropGeohash' exist?), which is cheaper than
-- reconstructing a 'DriverAreaPreference' per driver per batch just to test
-- membership. See 'SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.areaCheck'.
matchesRadius :: LatLong -> Meters -> LatLong -> Bool
matchesRadius center radius dropPoint =
  highPrecMetersToMeters (distanceBetweenInMeters center dropPoint) <= radius
