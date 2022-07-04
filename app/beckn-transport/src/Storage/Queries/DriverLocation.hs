{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Domain.Types.DriverLocation
import Domain.Types.Person
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> SqlDB ()
create drLocationId latLong updateTime =
  -- Tricky query to be able to insert meaningful Point
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val updateTime
        <#> val updateTime

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> SqlDB ()
upsertGpsCoord drLocationId latLong updateTime = do
  mbDrLoc <- Esq.findById @DriverLocation drLocationId
  case mbDrLoc of
    Nothing -> Storage.Queries.DriverLocation.create drLocationId latLong updateTime
    Just _ -> Esq.update $ \tbl -> do
      set
        tbl
        [ DriverLocationLat =. val latLong.lat,
          DriverLocationLon =. val latLong.lon,
          DriverLocationUpdatedAt =. val updateTime,
          DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon)
        ]
      where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
