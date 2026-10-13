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
-- A map from a key to a level. Keys:
--
-- * @"default"@ ('defaultKey'): any trip no special-location key applies to;
-- * @"Pickup_<specialLocationId>"@: trips that START at that special location;
-- * @"Drop_<specialLocationId>"@: trips that END there.
--
-- Ids are non-empty and contain no underscore. Which keys apply to a search is derived
-- from its Area in the driver app (@SharedLogic.FleetFeatures@), since beckn-spec does
-- not depend on special-zone where Area lives; 'resolvePoolingPriority' then picks the
-- level. A missing key means 'NO_PRIORITY'.
module Domain.Types.FleetPoolingPriority
  ( PoolingPriorityLevel (..),
    FleetPoolingPriority (..),
    defaultKey,
    pickupKeyPrefix,
    dropKeyPrefix,
    pickupKey,
    dropKey,
    isValidPoolingPriorityKey,
    invalidPoolingPriorityKeys,
    resolvePoolingPriority,
  )
where

import qualified Control.Lens as L
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.OpenApi (AdditionalProperties (..), NamedSchema (..), OpenApiType (..), ToSchema (..), additionalProperties, declareSchemaRef, description, type_)
import qualified Data.Text as T
import Kernel.Prelude

-- | Ordered NO_PRIORITY < LOW < MEDIUM < HIGH (the derived 'Ord'), which
-- 'resolvePoolingPriority' relies on. A missing value (column, row or key) means 'NO_PRIORITY'.
data PoolingPriorityLevel = NO_PRIORITY | LOW | MEDIUM | HIGH
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | JSON: a plain object, e.g.
-- @{"default": "LOW", "Drop_<id>": "HIGH", "Pickup_<id>": "MEDIUM"}@.
newtype FleetPoolingPriority = FleetPoolingPriority
  { getFleetPoolingPriority :: Map.Map Text PoolingPriorityLevel
  }
  deriving (Show, Eq, Generic)

instance FromJSON FleetPoolingPriority where
  parseJSON = fmap FleetPoolingPriority . parseJSON

instance ToJSON FleetPoolingPriority where
  toJSON = toJSON . getFleetPoolingPriority

instance ToSchema FleetPoolingPriority where
  declareNamedSchema _ = do
    levelSchema <- declareSchemaRef (Proxy :: Proxy PoolingPriorityLevel)
    pure $
      NamedSchema (Just "FleetPoolingPriority") $
        mempty
          & type_ L.?~ OpenApiObject
          & additionalProperties L.?~ AdditionalPropertiesSchema levelSchema
          & description L.?~ "Key -> PoolingPriorityLevel. Keys: \"default\", \"Pickup_<specialLocationId>\" (trip starts there), \"Drop_<specialLocationId>\" (trip ends there). Missing key = NO_PRIORITY."

-- | Applies when no special-location key applies to the search.
defaultKey :: Text
defaultKey = "default"

pickupKeyPrefix, dropKeyPrefix :: Text
pickupKeyPrefix = "Pickup_"
dropKeyPrefix = "Drop_"

pickupKey, dropKey :: Text -> Text
pickupKey = (pickupKeyPrefix <>)
dropKey = (dropKeyPrefix <>)

-- | 'defaultKey', or @Pickup_<id>@ / @Drop_<id>@ with one special location id. Ids are
-- non-empty and contain no underscore, so gated keys (and pickup-drop pair keys) are rejected.
isValidPoolingPriorityKey :: Text -> Bool
isValidPoolingPriorityKey key =
  key == defaultKey
    || any (\prefix -> maybe False validId (T.stripPrefix prefix key)) [pickupKeyPrefix, dropKeyPrefix]
  where
    validId slId = not (T.null slId) && not ("_" `T.isInfixOf` slId)

-- | Keys of the map that 'isValidPoolingPriorityKey' rejects.
invalidPoolingPriorityKeys :: FleetPoolingPriority -> [Text]
invalidPoolingPriorityKeys = filter (not . isValidPoolingPriorityKey) . Map.keys . getFleetPoolingPriority

-- | Level for one search and the key it came from. Of the candidate keys present in the
-- map, the one with the HIGHEST level wins (a tie goes to the earlier candidate); when
-- none is present, 'defaultKey' applies if set. Nothing when nothing applies, which
-- callers treat as 'NO_PRIORITY'.
resolvePoolingPriority :: [Text] -> FleetPoolingPriority -> Maybe (PoolingPriorityLevel, Text)
resolvePoolingPriority candidateKeys (FleetPoolingPriority priorities) =
  case mapMaybe lookupKey candidateKeys of
    [] -> lookupKey defaultKey
    firstMatch : otherMatches -> Just (foldl higher firstMatch otherMatches)
  where
    lookupKey k = (,k) <$> Map.lookup k priorities
    higher best candidate = if fst candidate > fst best then candidate else best
