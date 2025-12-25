{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.GateInfo where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Functions as F
import Kernel.Types.Id
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

deleteById :: Id D.GateInfo -> SqlDB ()
deleteById = Esq.deleteByKey @GateInfoT

deleteAll :: Id SL.SpecialLocation -> SqlDB ()
deleteAll specialLocationId =
  Esq.delete $ do
    gateInfo <- from $ table @GateInfoT
    where_ $
      gateInfo ^. GateInfoSpecialLocationId ==. val (toKey specialLocationId)
