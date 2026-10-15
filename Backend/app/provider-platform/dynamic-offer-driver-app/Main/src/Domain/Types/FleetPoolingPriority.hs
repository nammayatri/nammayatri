{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

data PoolingPriorityLevel = NO_PRIORITY | LOW | MEDIUM | HIGH
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | JSON object keyed by "Default", "Pickup_<specialLocationId>" or "Drop_<specialLocationId>".
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
          & description L.?~ "Area -> PoolingPriorityLevel. Keys: \"Default\", \"Pickup_<specialLocationId>\", \"Drop_<specialLocationId>\". Missing key = NO_PRIORITY."

-- | Highest level among the search's pickup and drop special-location entries (tie goes to
-- pickup), else the Default entry; also returns the matched key.
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
