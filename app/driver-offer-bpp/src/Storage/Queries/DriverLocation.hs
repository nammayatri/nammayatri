{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Domain.Types.DriverLocation
import Domain.Types.Person
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> SqlDB ()
create drLocationId latLong updateTime = do
  -- Tricky query to be able to insert meaningful Point
  now <- getCurrentTime
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val updateTime
        <#> val now
        <#> val now

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> SqlDB ()
upsertGpsCoord drLocationId latLong calculationTime = do
  mbDrLoc <- Esq.findById @DriverLocation @DriverLocationT drLocationId
  case mbDrLoc of
    Nothing -> Storage.Queries.DriverLocation.create drLocationId latLong calculationTime
    Just _ -> do
      now <- getCurrentTime
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverLocationLat =. val latLong.lat,
            DriverLocationLon =. val latLong.lon,
            DriverLocationCoordinatesCalculatedAt =. val calculationTime,
            DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon),
            DriverLocationUpdatedAt =. val now
          ]
        where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
