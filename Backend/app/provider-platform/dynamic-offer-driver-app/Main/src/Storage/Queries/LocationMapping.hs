{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.LocationMapping where

import Data.Tuple.Extra
import qualified Domain.Types.Location as Domain
import qualified Domain.Types.LocationMapping as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Storage.Tabular.Location as QLocation
import Storage.Tabular.LocationMapping

create :: Domain.LocationMapping -> SqlDB ()
create locationMapping =
  Esq.withFullEntity locationMapping $ \(locationMappingT, locationT) -> do
    Esq.create' locationMappingT
    Esq.create' locationT

createOnlyMapping :: Domain.LocationMapping -> SqlDB ()
createOnlyMapping locationMapping =
  Esq.withFullEntity locationMapping $ \(locationMappingT, _) -> do
    Esq.create' locationMappingT

updateLocationInMapping :: Domain.LocationMapping -> Domain.Location -> SqlDB ()
updateLocationInMapping mapping newLocation = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ LocationMappingLocationId =. val (toKey newLocation.id)
      ]
    where_ $ tbl ^. LocationMappingTId ==. val (toKey mapping.id)

createMany :: [Domain.LocationMapping] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let locationMappingTs = map fst list
        locationTs = map fst list
    Esq.createMany' locationMappingTs
    Esq.createMany' locationTs

putLocationMappingTs :: [FullLocationMappingT] -> FullEntitySqlDB ()
putLocationMappingTs toLocT = do
  let locationMappingT = map fst toLocT
  let locationT = map snd toLocT
  Esq.createMany' locationMappingT
  Esq.createMany' locationT

fullLocationMappingTable ::
  From
    ( Table LocationMappingT
        :& Table QLocation.LocationT
    )
fullLocationMappingTable =
  table @LocationMappingT
    `innerJoin` table @QLocation.LocationT
      `Esq.on` ( \(locationMapping :& mbLocation) ->
                   locationMapping ^. LocationMappingLocationId ==. mbLocation ^. QLocation.LocationTId
               )

findByTagId :: Transactionable m => Text -> m [Domain.LocationMapping]
findByTagId tagId = Esq.buildDType $ do
  mbFullLocationMappingT <- Esq.findAll' $ do
    (locationMapping :& location) <- from fullLocationMappingTable
    where_ $ locationMapping ^. LocationMappingTagId ==. val tagId
    pure (locationMapping, location)
  mapM buildFullLocationMapping mbFullLocationMappingT

findByTagIdAndOrder :: Transactionable m => Text -> Int -> m (Maybe Domain.LocationMapping)
findByTagIdAndOrder tagId order = Esq.buildDType $ do
  mbFullLocationMappingT <- Esq.findOne' $ do
    (locationMapping :& location) <- from fullLocationMappingTable
    where_ $
      locationMapping ^. LocationMappingTagId ==. val tagId
        &&. locationMapping ^. LocationMappingOrder ==. val order
    pure (locationMapping, location)
  mapM buildFullLocationMapping mbFullLocationMappingT

buildFullLocationMapping ::
  Transactionable m =>
  (LocationMappingT, QLocation.LocationT) ->
  DTypeBuilder m (SolidType FullLocationMappingT)
buildFullLocationMapping (locationMappingT, locationT) = do
  return $ extractSolidType @Domain.LocationMapping (locationMappingT, locationT)
