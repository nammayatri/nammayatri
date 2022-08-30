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
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.ClassOfVehicle"
derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertificateT sql=vehicle_registration_certificate
      id Text
      driverId PersonTId
      certificateNumber Text
      fitnessExpiry UTCTime Maybe
      permitNumber Text Maybe
      permitStart UTCTime Maybe
      permitExpiry UTCTime Maybe
      pucExpiry UTCTime Maybe
      vehicleClass Domain.ClassOfVehicle Maybe
      vehicleColor Text Maybe
      vehicleManufacturer Text Maybe
      vehicleModel Text Maybe
      insuranceValidity  UTCTime Maybe
      idfyRequestId Text Maybe
      idfyResponseDump Text Maybe
      verificationStatus Domain.VerificationStatus
      version Int
      active Bool
      consent Bool
      consentTimestamp UTCTime
      createdAt UTCTime
      updatedAt UTCTime
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
          driverId = fromKey driverId,
          certificateNumber = EncryptedHashed (Encrypted certificateNumber) (DbHash BS.empty),
          ..
        }
  toTType Domain.VehicleRegistrationCertificate {..} =
    VehicleRegistrationCertificateT
      { id = getId id,
        driverId = toKey driverId,
        certificateNumber = certificateNumber & unEncrypted . (.encrypted),
        ..
      }
