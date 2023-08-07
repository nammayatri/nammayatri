{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SpecialZone where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Domain.Types.SpecialZone
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.SpecialZone

create :: SpecialZone -> Geometry -> SqlDB ()
create zone geometry = do
  now <- getCurrentTime
  Esq.insertSelect $
    return $
      SpecialZoneT
        <# val (getId zone.id)
        <#> val zone.name
        <#> val zone.categoryCode
        <#> val zone.geoJson
        <#> geojsonToBin (val (TE.decodeUtf8 $ BSL.toStrict $ encode geometry))
        <#> val zone.city
        <#> val zone.state
        <#> val now
        <#> val now

findById :: Transactionable m => Id SpecialZone -> m (Maybe SpecialZone)
findById = Esq.findById

update :: SpecialZone -> Geometry -> SqlDB ()
update SpecialZone {..} geometry = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SpecialZoneName =. val name,
        SpecialZoneCategoryCode =. val categoryCode,
        SpecialZoneCity =. val city,
        SpecialZoneState =. val state,
        SpecialZoneGeoJson =. val geoJson,
        SpecialZoneGeom =. geojsonToBin (val (TE.decodeUtf8 $ BSL.toStrict $ encode geometry)),
        SpecialZoneUpdatedAt =. val now
      ]
    where_ $ tbl ^. SpecialZoneTId ==. val (toKey id)

findSpecialZoneByLatLong :: Transactionable m => LatLong -> m (Maybe SpecialZone)
findSpecialZoneByLatLong point = do
  Esq.findOne $ do
    specialZone <- from $ table @SpecialZoneT
    where_ $ containsPointGeom (point.lon, point.lat)
    return specialZone

findSpecialZonesByRegion :: Transactionable m => LatLong -> LatLong -> m [SpecialZone]
findSpecialZonesByRegion minPoint maxPoint = do
  Esq.findAll $ do
    specialZones <- from $ table @SpecialZoneT
    where_ $ containsRegion (minPoint.lon, minPoint.lat) (maxPoint.lon, maxPoint.lat)
    pure specialZones

deleteById :: Id SpecialZone -> SqlDB ()
deleteById = Esq.deleteByKey @SpecialZoneT
