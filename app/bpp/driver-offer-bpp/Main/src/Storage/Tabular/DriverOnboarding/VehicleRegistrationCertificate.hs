{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import Storage.Tabular.DriverOnboarding.Image (ImageTId)

derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertificateT sql=vehicle_registration_certificate
      id Text
      documentImageId ImageTId
      certificateNumberEncrypted Text
      certificateNumberHash DbHash
      fitnessExpiry UTCTime
      permitExpiry UTCTime Maybe
      pucExpiry UTCTime Maybe
      insuranceValidity  UTCTime Maybe
      vehicleClass Text Maybe
      vehicleManufacturer Text Maybe
      vehicleCapacity Int Maybe
      vehicleModel Text Maybe
      vehicleColor Text Maybe
      vehicleEnergyType Text Maybe
      verificationStatus Domain.VerificationStatus
      failedRules (PostgresList Text)
      createdAt UTCTime
      updatedAt UTCTime
      UniqueVehicleRegistrationCertificateRCId certificateNumberHash fitnessExpiry
      Primary id
      deriving Generic
    |]

instance TEntityKey VehicleRegistrationCertificateT where
  type DomainKey VehicleRegistrationCertificateT = Id Domain.VehicleRegistrationCertificate
  fromKey (VehicleRegistrationCertificateTKey _id) = Id _id
  toKey (Id id) = VehicleRegistrationCertificateTKey id

instance TType VehicleRegistrationCertificateT Domain.VehicleRegistrationCertificate where
  fromTType VehicleRegistrationCertificateT {..} = do
    return $
      Domain.VehicleRegistrationCertificate
        { id = Id id,
          documentImageId = fromKey documentImageId,
          certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
          failedRules = unPostgresList failedRules,
          ..
        }
  toTType Domain.VehicleRegistrationCertificate {..} =
    VehicleRegistrationCertificateT
      { id = getId id,
        documentImageId = toKey documentImageId,
        certificateNumberEncrypted = certificateNumber & unEncrypted . (.encrypted),
        certificateNumberHash = certificateNumber & (.hash),
        failedRules = PostgresList failedRules,
        ..
      }
