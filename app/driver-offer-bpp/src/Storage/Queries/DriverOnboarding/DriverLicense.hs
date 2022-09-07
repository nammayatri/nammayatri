{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.Person ()

create :: DriverLicense -> SqlDB ()
create = Esq.create

upsert :: DriverLicense -> SqlDB ()
upsert a@DriverLicense {..} =
  Esq.upsert
    a
    [ DriverLicenseDriverDob =. val driverDob,
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
  Transactionable m =>
  EncryptedHashedField 'AsEncrypted Text ->
  m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  findOne $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseLicenseNumber ==. val (dlNumber & unEncrypted . (.encrypted))
    return dl
