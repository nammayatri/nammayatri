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
import Domain.Types.Location as Domain
import Domain.Types.LocationMapping as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Location
import Storage.Tabular.LocationMapping
import Kernel.Types.Id

updateLocationInMapping :: Domain.LocationMapping -> Id Domain.Location -> SqlDB ()
updateLocationInMapping mapping newLocationId = do
  version <- Domain.getMappingVersion
  Esq.update $ \tbl -> do
    set
      tbl
      [ LocationMappingLocationId =. val (toKey newLocationId),
        LocationMappingVersion =. val (show version)
      ]
    where_ $ tbl ^. LocationMappingTId ==. val (toKey mapping.id)

create :: LocationMapping -> SqlDB ()
create locationMapping =
  Esq.withFullEntity locationMapping $ \(locationMappingT, _) -> do
    Esq.create' locationMappingT

createMany :: [LocationMapping] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let locationMappingTs = map fst list
    Esq.createMany' locationMappingTs

putLocationMappingTs :: [FullLocationMappingT] -> FullEntitySqlDB ()
putLocationMappingTs toLocT = do
  let locationMappingT = map fst toLocT
  let locationT = map snd toLocT
  Esq.createMany' locationMappingT
  Esq.createMany' locationT

fullLocationMappingTable ::
  From
    ( Table LocationMappingT
        :& Table LocationT
    )
fullLocationMappingTable =
  table @LocationMappingT
    `innerJoin` table @LocationT
      `Esq.on` ( \(locationMapping :& mbLocation) ->
                   locationMapping ^. LocationMappingLocationId ==. mbLocation ^. LocationTId
               )

findByTagId :: Transactionable m => Text -> m [Domain.LocationMapping]
findByTagId tagId = Esq.buildDType $ do
  mbFullLocationMappingT <- Esq.findAll' $ do
    (locationMapping :& location) <- from fullLocationMappingTable
    where_ $ locationMapping ^. LocationMappingTagId ==. val tagId
    pure (locationMapping, location)
  mapM buildFullLocationMapping mbFullLocationMappingT

buildFullLocationMapping ::
  Transactionable m =>
  (LocationMappingT, LocationT) ->
  DTypeBuilder m (SolidType FullLocationMappingT)
buildFullLocationMapping (locationMappingT, locationT) = do
  return $ extractSolidType @LocationMapping (locationMappingT, locationT)

findByTagIdAndOrder :: Transactionable m => Text -> Int -> m (Maybe Domain.LocationMapping)
findByTagIdAndOrder tagId order = Esq.buildDType $ do
  mbFullLocationMappingT <- Esq.findOne' $ do
    (locationMapping :& location) <- from fullLocationMappingTable
    where_ $
      locationMapping ^. LocationMappingTagId ==. val tagId
        &&. locationMapping ^. LocationMappingOrder ==. val order
    pure (locationMapping, location)
  mapM buildFullLocationMapping mbFullLocationMappingT
