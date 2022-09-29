{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person ()

create :: VehicleRegistrationCertificate -> SqlDB ()
create = Esq.create

upsert :: VehicleRegistrationCertificate -> SqlDB ()
upsert a@VehicleRegistrationCertificate {..} =
  Esq.upsert
    a
    [ VehicleRegistrationCertificatePermitExpiry =. val permitExpiry,
      VehicleRegistrationCertificatePucExpiry =. val pucExpiry,
      VehicleRegistrationCertificateInsuranceValidity =. val insuranceValidity,
      VehicleRegistrationCertificateVehicleClass =. val vehicleClass,
      VehicleRegistrationCertificateVehicleManufacturer =. val vehicleManufacturer,
      VehicleRegistrationCertificateVerificationStatus =. val verificationStatus,
      VehicleRegistrationCertificateFailedRules =. val (PostgresList failedRules),
      VehicleRegistrationCertificateUpdatedAt =. val updatedAt
    ]

findById ::
  EsqDBFlow m r =>
  Id VehicleRegistrationCertificate ->
  m (Maybe VehicleRegistrationCertificate)
findById = Esq.findById

findLastVehicleRC ::
  Transactionable m =>
  EncryptedHashedField 'AsEncrypted Text ->
  m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumber = do
  rcs <- findAll $ do
    rc <- from $ table @VehicleRegistrationCertificateT
    where_ $ rc ^. VehicleRegistrationCertificateCertificateNumber ==. val (certNumber & unEncrypted . (.encrypted))
    orderBy [desc $ rc ^. VehicleRegistrationCertificateFitnessExpiry]
    return rc
  pure $ headMaybe rcs
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

findByRCAndExpiry ::
  Transactionable m =>
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  findOne $ do
    rc <- from $ table @VehicleRegistrationCertificateT
    where_ $
      rc ^. VehicleRegistrationCertificateCertificateNumber ==. val (certNumber & unEncrypted . (.encrypted))
        &&. rc ^. VehicleRegistrationCertificateFitnessExpiry ==. val expiry
    return rc
