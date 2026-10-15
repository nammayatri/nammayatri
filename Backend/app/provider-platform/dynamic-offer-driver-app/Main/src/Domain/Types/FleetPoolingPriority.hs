{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    isSupportedPoolingPriorityArea,
    resolvePoolingPriority,
  )
where

import qualified Data.Map.Strict as Map
import Domain.Types.Extra.SpecialLocationArea ()
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import qualified Lib.Types.SpecialLocation as SL

data PoolingPriorityLevel = NO_PRIORITY | LOW | MEDIUM | HIGH
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | JSON object keyed by "Default", "Pickup_<specialLocationId>" or "Drop_<specialLocationId>".
newtype FleetPoolingPriority = FleetPoolingPriority
  { getFleetPoolingPriority :: Map.Map SL.Area PoolingPriorityLevel
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

-- | Keys a fleet may configure: Default, a gate-less Pickup, or a Drop.
isSupportedPoolingPriorityArea :: SL.Area -> Bool
isSupportedPoolingPriorityArea = \case
  SL.Default -> True
  SL.Pickup _ Nothing -> True
  SL.Drop _ -> True
  _ -> False

-- | Highest level among the search's pickup and drop special-location entries, else the Default entry.
resolvePoolingPriority :: Maybe SL.Area -> FleetPoolingPriority -> Maybe PoolingPriorityLevel
resolvePoolingPriority mbArea (FleetPoolingPriority priorities) =
  case mapMaybe (`Map.lookup` priorities) candidates of
    [] -> Map.lookup SL.Default priorities
    levels -> Just (maximum levels)
  where
    candidates =
      maybeToList ((\pickupId -> SL.Pickup (Id pickupId) Nothing) <$> (mbArea >>= SL.pickupSpecialZoneIdFromArea))
        <> maybeToList (SL.Drop . Id <$> (mbArea >>= SL.dropSpecialZoneIdFromArea))
