{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Vehicle as Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.Id
import Kernel.Utils.IOLogging (LoggerEnv)
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
      VehicleRegistrationCertificateVehicleVariant =. val vehicleVariant,
      VehicleRegistrationCertificateVehicleManufacturer =. val vehicleManufacturer,
      VehicleRegistrationCertificateVehicleCapacity =. val vehicleCapacity,
      VehicleRegistrationCertificateVehicleModel =. val vehicleModel,
      VehicleRegistrationCertificateVehicleColor =. val vehicleColor,
      VehicleRegistrationCertificateVehicleEnergyType =. val vehicleEnergyType,
      VehicleRegistrationCertificateVerificationStatus =. val verificationStatus,
      VehicleRegistrationCertificateFailedRules =. val (PostgresList failedRules),
      VehicleRegistrationCertificateUpdatedAt =. val updatedAt
    ]

findById ::
  Transactionable m =>
  Id VehicleRegistrationCertificate ->
  m (Maybe VehicleRegistrationCertificate)
findById = Esq.findById

findLastVehicleRC ::
  (Transactionable m) =>
  DbHash ->
  m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumberHash = do
  rcs <- findAll $ do
    rc <- from $ table @VehicleRegistrationCertificateT
    where_ $ rc ^. VehicleRegistrationCertificateCertificateNumberHash ==. val certNumberHash
    orderBy [desc $ rc ^. VehicleRegistrationCertificateFitnessExpiry]
    return rc
  pure $ headMaybe rcs
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

updateVehicleVariant :: Id VehicleRegistrationCertificate -> Maybe Vehicle.Variant -> SqlDB ()
updateVehicleVariant id variant = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleRegistrationCertificateVehicleVariant =. val variant
      ]
    where_ $
      tbl ^. VehicleRegistrationCertificateId ==. val (id.getId)

findByRCAndExpiry ::
  Transactionable m =>
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  let certNumberHash = certNumber & (.hash)
  findOne $ do
    rc <- from $ table @VehicleRegistrationCertificateT
    where_ $
      rc ^. VehicleRegistrationCertificateCertificateNumberHash ==. val certNumberHash
        &&. rc ^. VehicleRegistrationCertificateFitnessExpiry ==. val expiry
    return rc

findAllById :: Transactionable m => [Id VehicleRegistrationCertificate] -> m [VehicleRegistrationCertificate]
findAllById rcIds =
  findAll $ do
    rc <- from $ table @VehicleRegistrationCertificateT
    where_ $ rc ^. VehicleRegistrationCertificateId `in_` valList (map (.getId) rcIds)
    return rc

findLastVehicleRCWrapper ::
  ( Transactionable m,
    EncFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    HasField "loggerEnv" r LoggerEnv
  ) =>
  Text ->
  m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWrapper certNumber = do
  certNumberHash <- getDbHash certNumber
  Esq.runInReplica $ findLastVehicleRC certNumberHash
