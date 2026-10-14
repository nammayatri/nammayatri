{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet-level pooling priority, stored on @fleet_config.fleet_pooling_priority@,
-- edited through the fleet driver-controls dashboard API and handed to the POOLING
-- dynamic logic on every driver of the fleet.
--
-- A map from a search 'SL.Area' to a level. Supported areas (JSON keys are the Area's
-- rendering):
--
-- * @Default@: any trip no special-location entry applies to;
-- * @Pickup <id> Nothing@ (@"Pickup_<specialLocationId>"@): trips that START there;
-- * @Drop <id>@ (@"Drop_<specialLocationId>"@): trips that END there.
--
-- Gated pickups and pickup-drop pairs are rejected on decode: a search's area carries a
-- pair only when a PickupDrop fare product exists, so a pair entry would usually never
-- apply. 'resolvePoolingPriority' picks the level for a search. A missing entry means
-- 'NO_PRIORITY'.
module Domain.Types.FleetPoolingPriority
  ( PoolingPriorityLevel (..),
    FleetPoolingPriority (..),
    resolvePoolingPriority,
  )
where

import qualified Control.Lens as L
import Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.OpenApi (AdditionalProperties (..), NamedSchema (..), OpenApiType (..), ToSchema (..), additionalProperties, declareSchemaRef, description, type_)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import qualified Lib.Types.SpecialLocation as SL

-- | Ordered NO_PRIORITY < LOW < MEDIUM < HIGH (the derived 'Ord'), which
-- 'resolvePoolingPriority' relies on. A missing value (column, row or entry) means 'NO_PRIORITY'.
data PoolingPriorityLevel = NO_PRIORITY | LOW | MEDIUM | HIGH
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | JSON: a plain object keyed by the Area's rendering, e.g.
-- @{"Default": "LOW", "Drop_<id>": "HIGH", "Pickup_<id>": "MEDIUM"}@.
newtype FleetPoolingPriority = FleetPoolingPriority
  { getFleetPoolingPriority :: Map.Map SL.Area PoolingPriorityLevel
  }
  deriving (Show, Eq, Generic)

instance ToJSON FleetPoolingPriority where
  toJSON (FleetPoolingPriority priorities) =
    Object . KeyMap.fromList $ [(AesonKey.fromString (show area), toJSON level) | (area, level) <- Map.toList priorities]

instance FromJSON FleetPoolingPriority where
  parseJSON = withObject "FleetPoolingPriority" $ \obj ->
    FleetPoolingPriority . Map.fromList <$> mapM parseEntry (KeyMap.toList obj)
    where
      parseEntry (key, value) = do
        area <- parseJSON (String (AesonKey.toText key))
        unless (isSupportedArea area) $
          fail $ "Unsupported fleetPoolingPriority key " <> show (AesonKey.toText key) <> ": expected \"Default\", \"Pickup_<specialLocationId>\" or \"Drop_<specialLocationId>\""
        (area,) <$> parseJSON value
      isSupportedArea = \case
        SL.Default -> True
        SL.Pickup _ Nothing -> True
        SL.Drop _ -> True
        _ -> False

instance ToSchema FleetPoolingPriority where
  declareNamedSchema _ = do
    levelSchema <- declareSchemaRef (Proxy :: Proxy PoolingPriorityLevel)
    pure $
      NamedSchema (Just "FleetPoolingPriority") $
        mempty
          & type_ L.?~ OpenApiObject
          & additionalProperties L.?~ AdditionalPropertiesSchema levelSchema
          & description L.?~ "Area -> PoolingPriorityLevel. Keys: \"Default\", \"Pickup_<specialLocationId>\" (trip starts there), \"Drop_<specialLocationId>\" (trip ends there). Missing key = NO_PRIORITY."

-- | Level for a search with this area, and the entry it came from (rendered as its key).
-- Candidates are the search's pickup and drop special locations (gate ids ignored); of
-- those present in the map, the HIGHEST level wins (a tie goes to the pickup). When none
-- is present, the 'SL.Default' entry applies if set. Nothing when nothing applies, which
-- callers treat as 'NO_PRIORITY'.
resolvePoolingPriority :: Maybe SL.Area -> FleetPoolingPriority -> Maybe (PoolingPriorityLevel, Text)
resolvePoolingPriority mbArea (FleetPoolingPriority priorities) =
  case mapMaybe lookupArea candidates of
    [] -> lookupArea SL.Default
    firstMatch : otherMatches -> Just (foldl higher firstMatch otherMatches)
  where
    candidates =
      maybeToList ((\pickupId -> SL.Pickup (Id pickupId) Nothing) <$> (mbArea >>= SL.pickupSpecialZoneIdFromArea))
        <> maybeToList (SL.Drop . Id <$> (mbArea >>= SL.dropSpecialZoneIdFromArea))
    lookupArea area = (,show area) <$> Map.lookup area priorities
    higher best candidate = if fst candidate > fst best then candidate else best
