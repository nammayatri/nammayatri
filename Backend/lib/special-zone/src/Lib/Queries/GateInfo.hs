{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.GateInfo where

import Data.List (sortBy)
import Data.Ord (comparing)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Functions as F
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Lib.Tabular.GateInfo
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL

findById :: Transactionable m => Id D.GateInfo -> m (Maybe D.GateInfo)
findById = Esq.findById

findAllGatesBySpecialLocationId :: Transactionable m => Id SL.SpecialLocation -> m [(D.GateInfo, Maybe Text)]
findAllGatesBySpecialLocationId slId = Esq.findAll $ do
  gateInfo <- from $ table @GateInfoT
  where_ $ gateInfo ^. GateInfoSpecialLocationId ==. val (toKey slId)
  return (gateInfo, F.mbGetGeomGeoJSON)

findAllGatesBySpecialLocationIdWithoutGeoJson :: Transactionable m => Id SL.SpecialLocation -> m [D.GateInfo]
findAllGatesBySpecialLocationIdWithoutGeoJson slId = Esq.findAll $ do
  gateInfo <- from $ table @GateInfoT
  where_ $ gateInfo ^. GateInfoSpecialLocationId ==. val (toKey slId)
  return gateInfo

findGateInfoIfDriverInsideGatePickupZone :: Transactionable m => LatLong -> m (Maybe D.GateInfo)
findGateInfoIfDriverInsideGatePickupZone point = do
  Esq.findOne $ do
    gateInfo <- from $ table @GateInfoT
    where_ $ containsPoint (point.lon, point.lat)
    return gateInfo

findGateInfoByLatLongWithoutGeoJson :: Transactionable m => LatLong -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithoutGeoJson point = do
  Esq.findOne $ do
    gateInfo <- from $ table @GateInfoT
    where_ $ gateInfo ^. GateInfoPoint ==. val point
    return gateInfo

-- | Find the nearest gate info within a given radius (in meters) of the given point,
-- scoped to a specific special location. Uses Haversine distance since the `point`
-- column is a serialized LatLong, not PostGIS geometry.
-- Returns the closest matching gate, or Nothing if none is within the radius.
findGateInfoByLatLongWithinRadius :: Transactionable m => Id SL.SpecialLocation -> LatLong -> Double -> m (Maybe D.GateInfo)
findGateInfoByLatLongWithinRadius slId point radiusMeters = do
  gates <- Esq.findAll $ do
    gateInfo <- from $ table @GateInfoT
    where_ $ gateInfo ^. GateInfoSpecialLocationId ==. val (toKey slId)
    return gateInfo
  -- Precompute distance once per gate and tie-break by gate id so the
  -- selection is deterministic when two gates sit at the same distance.
  let gatesWithDist = map (\g -> (distanceBetweenInMeters point g.point, g)) gates
      gatesInRadius = filter (\(d, _) -> d <= realToFrac radiusMeters) gatesWithDist
      sorted = sortBy (comparing (\(d, g) -> (d, g.id))) gatesInRadius
  return $ snd <$> listToMaybe sorted

deleteById :: Id D.GateInfo -> SqlDB ()
deleteById = Esq.deleteByKey @GateInfoT

findGatesWithDriverThreshold :: Transactionable m => Id SL.SpecialLocation -> m [D.GateInfo]
findGatesWithDriverThreshold slId = Esq.findAll $ do
  gateInfo <- from $ table @GateInfoT
  where_ $
    gateInfo ^. GateInfoSpecialLocationId ==. val (toKey slId)
      &&. gateInfo ^. GateInfoCanQueueUpOnGate ==. val True
      &&. ( not_ (isNothing (gateInfo ^. GateInfoMinDriverThresholdsJson))
              ||. not_ (isNothing (gateInfo ^. GateInfoDefaultMinDriverThreshold))
          )
  return gateInfo

deleteAll :: Id SL.SpecialLocation -> SqlDB ()
deleteAll specialLocationId =
  Esq.delete $ do
    gateInfo <- from $ table @GateInfoT
    where_ $
      gateInfo ^. GateInfoSpecialLocationId ==. val (toKey specialLocationId)
