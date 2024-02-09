{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.SpecialLocation where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Functions as F
import Kernel.Types.Id
import qualified Lib.Queries.GateInfo as QGI
import Lib.Tabular.SpecialLocation
import qualified Lib.Types.GateInfo as GD
import qualified Lib.Types.SpecialLocation as D

data SpecialLocationFull = SpecialLocationFull
  { id :: Id D.SpecialLocation,
    locationName :: Text,
    category :: Text,
    merchantOperatingCityId :: Maybe Text,
    gatesInfo :: [GD.GateInfoFull],
    gates :: [D.GatesInfo], --TODO: deprecate this later
    geoJson :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

create :: D.SpecialLocation -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id D.SpecialLocation -> m (Maybe D.SpecialLocation)
findById = Esq.findById

makeFullSpecialLocation :: Transactionable m => (D.SpecialLocation, Text) -> m SpecialLocationFull
makeFullSpecialLocation (D.SpecialLocation {..}, specialShape) = do
  gatesWithShape <- QGI.findAllGatesBySpecialLocationId id
  let gatesInfoFull = map (\(GD.GateInfo {point = gatePoint, id = gateId, createdAt = _gateCreatedAt, ..}, gateShape) -> GD.GateInfoFull {GD.id = gateId, GD.point = gatePoint, GD.geoJson = gateShape, ..}) gatesWithShape
  pure $ SpecialLocationFull {gatesInfo = gatesInfoFull, geoJson = Just specialShape, ..}

findFullSpecialLocationsByMerchantOperatingCityId :: Transactionable m => Text -> m [SpecialLocationFull]
findFullSpecialLocationsByMerchantOperatingCityId mocId = do
  mbRes <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $
        specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
          ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation mbRes

findSpecialLocationByLatLongFull :: Transactionable m => LatLong -> Int -> m (Maybe SpecialLocationFull)
findSpecialLocationByLatLongFull point radius = do
  mbRes <-
    Esq.findOne $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ pointCloseByOrWithin (point.lon, point.lat) (val radius)
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation mbRes

findSpecialLocationByLatLongNearby :: Transactionable m => LatLong -> Int -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLongNearby point radius = do
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $ pointCloseByOrWithin (point.lon, point.lat) (val radius)
    return (specialLocation, F.getGeomGeoJSON)

findPickupSpecialLocationByLatLong :: Transactionable m => LatLong -> m (Maybe D.SpecialLocation)
findPickupSpecialLocationByLatLong point = do
  mbSpecialLocation <- findSpecialLocationByLatLong' point
  case mbSpecialLocation of
    Just specialLocation -> pure $ Just specialLocation
    Nothing -> maybe (pure Nothing) (Lib.Queries.SpecialLocation.findById . (.specialLocationId)) =<< QGI.findGateInfoByLatLongWithoutGeoJson point

findSpecialLocationByLatLong' :: Transactionable m => LatLong -> m (Maybe D.SpecialLocation)
findSpecialLocationByLatLong' point = do
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $ containsPoint (point.lon, point.lat)
    return specialLocation

findSpecialLocationByLatLong :: Transactionable m => LatLong -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLong point = do
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $ containsPoint (point.lon, point.lat)
    return (specialLocation, F.getGeomGeoJSON)
