{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driveronboarding.VehicleRegistrationCert where
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as Domain
import Beckn.Types.Id
import Storage.Tabular.Person (PersonTId)
import qualified Data.ByteString as BS
import Beckn.External.Encryption



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
      insuranceValidity  UTCTime Maybe
      vehicleClass Domain.COV Maybe
      vehicleNumber Text Maybe
      idfyStatus Domain.IdfyStatus
      verificationStatus Domain.VerificationStatus
      request_id Text
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



    