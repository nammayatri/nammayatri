{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Driver.GoHomeFeature.DriverHomeLocation where

import Domain.Types.Driver.GoHomeFeature.DriverHomeLocation
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Driver.GoHomeFeature.DriverHomeLocation

create :: DriverHomeLocation -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverHomeLocation ->
  m (Maybe DriverHomeLocation)
findById = Esq.findById

findAllByDriverId :: Transactionable m => Id Driver -> m [DriverHomeLocation]
findAllByDriverId driverId = do
  Esq.findAll $ do
    driverHomeLocation <- from $ table @DriverHomeLocationT
    where_ $ driverHomeLocation ^. DriverHomeLocationDriverId ==. val (toKey driverId)
    return driverHomeLocation

deleteById :: Id DriverHomeLocation -> SqlDB ()
deleteById = deleteByKey @DriverHomeLocationT

deleteByDriverId :: Id Driver -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    driverHomeLocation <- from $ table @DriverHomeLocationT
    where_ $ driverHomeLocation ^. DriverHomeLocationDriverId ==. val (toKey $ cast driverId)
