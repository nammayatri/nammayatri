{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Beckn.External.Maps.Types (LatLong (..))
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id
import Domain.Types.DriverLocation
import Domain.Types.Person
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> SqlDB ()
create drLocationId latLong coordinatesUpdatedAt = do
  now <- getCurrentTime
  -- Tricky query to be able to insert meaningful Point
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val coordinatesUpdatedAt
        <#> val now
        <#> val now

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> SqlDB ()
upsertGpsCoord drLocationId latLong coordinatesUpdatedAt = do
  mbDrLoc <- Esq.findById @DriverLocation drLocationId
  case mbDrLoc of
    Nothing -> Storage.Queries.DriverLocation.create drLocationId latLong coordinatesUpdatedAt
    Just _ -> do
      now <- getCurrentTime
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverLocationLat =. val latLong.lat,
            DriverLocationLon =. val latLong.lon,
            DriverLocationCoordinatesCalculatedAt =. val coordinatesUpdatedAt,
            DriverLocationUpdatedAt =. val now,
            DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon)
          ]
        where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)

deleteById :: Id Person -> SqlDB ()
deleteById = deleteByKey @DriverLocationT
