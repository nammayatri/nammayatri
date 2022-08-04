{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driveronboarding.VehicleRegistrationCert where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Data.ByteString as BS
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as Domain
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.COV"
derivePersistField "Domain.IdfyStatus"
derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertT sql = _vehicle_registration_cert_t
      id Text
      driverId PersonTId
      vehicleRegistrationCertNumber Text Maybe
      fitnessCertExpiry UTCTime Maybe
      permitNumber Text Maybe
      permitStart UTCTime Maybe
      permitExpiry UTCTime Maybe
      pucExpiry UTCTime Maybe
      vehicleClass Domain.COV Maybe
      vehicleNumber Text Maybe
      vehicleColor Text Maybe 
      vehicleManufacturer Text Maybe
      vehicleModel Text Maybe
      insuranceValidity  UTCTime Maybe
      idfyRequestId Text 
      verificationRespDump Text
      idfyStatus Domain.IdfyStatus
      verificationStatus Domain.VerificationStatus
      rcImageS3Path Text
      consent Bool
      consentTimestamp UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey VehicleRegistrationCertT where
  type DomainKey VehicleRegistrationCertT = Id Domain.VehicleRegistrationCert
  fromKey (VehicleRegistrationCertTKey _id) = Id _id
  toKey (Id id) = VehicleRegistrationCertTKey id

instance TType VehicleRegistrationCertT Domain.VehicleRegistrationCert where
  fromTType VehicleRegistrationCertT {..} = do
    return $
      Domain.VehicleRegistrationCert
        { id = Id id,
          driverId = fromKey driverId,
          vehicleRegistrationCertNumber = EncryptedHashed <$> (Encrypted <$> vehicleRegistrationCertNumber) <*> Just (DbHash BS.empty),
          ..
        }
  toTType Domain.VehicleRegistrationCert {..} =
    VehicleRegistrationCertT
      { id = getId id,
        driverId = toKey driverId,
        vehicleRegistrationCertNumber = vehicleRegistrationCertNumber <&> unEncrypted . (.encrypted),
        ..
      }
