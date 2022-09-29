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
import qualified Data.ByteString as BS
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain

derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertificateT sql=vehicle_registration_certificate
      id Text
      certificateNumber Text
      fitnessExpiry UTCTime
      permitExpiry UTCTime Maybe
      pucExpiry UTCTime Maybe
      insuranceValidity  UTCTime Maybe
      vehicleClass Text Maybe
      vehicleManufacturer Text Maybe
      verificationStatus Domain.VerificationStatus
      failedRules (PostgresList Text)
      createdAt UTCTime
      updatedAt UTCTime
      UniqueVehicleRegistrationCertificateRCId certificateNumber fitnessExpiry
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
          certificateNumber = EncryptedHashed (Encrypted certificateNumber) (DbHash BS.empty),
          failedRules = unPostgresList failedRules,
          ..
        }
  toTType Domain.VehicleRegistrationCertificate {..} =
    VehicleRegistrationCertificateT
      { id = getId id,
        certificateNumber = certificateNumber & unEncrypted . (.encrypted),
        failedRules = PostgresList failedRules,
        ..
      }
