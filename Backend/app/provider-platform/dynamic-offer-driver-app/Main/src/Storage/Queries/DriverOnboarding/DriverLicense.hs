{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Encryption
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.Person ()

create :: DriverLicense -> SqlDB ()
create = Esq.create

upsert :: DriverLicense -> SqlDB ()
upsert a@DriverLicense {..} =
  Esq.upsert
    a
    [ DriverLicenseDriverDob =. val driverDob,
      DriverLicenseDriverName =. val driverName,
      DriverLicenseLicenseExpiry =. val licenseExpiry,
      DriverLicenseClassOfVehicles =. val (PostgresList classOfVehicles),
      DriverLicenseVerificationStatus =. val verificationStatus,
      DriverLicenseFailedRules =. val (PostgresList failedRules),
      DriverLicenseUpdatedAt =. val updatedAt
    ]

findById ::
  Transactionable m =>
  Id DriverLicense ->
  m (Maybe DriverLicense)
findById = Esq.findById

findByDriverId ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLicense)
findByDriverId driverId = do
  findOne $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseDriverId ==. val (toKey driverId)
    return dl

findByDLNumber ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dlNumberHash <- getDbHash dlNumber
  findOne $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseLicenseNumberHash ==. val dlNumberHash
    return dl

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseDriverId ==. val (toKey driverId)
