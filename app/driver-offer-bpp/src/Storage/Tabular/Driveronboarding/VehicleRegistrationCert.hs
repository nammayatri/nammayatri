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



derivePersistField "Domain.VehicleClass"
derivePersistField "Domain.Verification2"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertT sql = vehicle_registrationcert
      id Text
      driverId PersonTId
      vehicleRegistrationCertNumber Text Maybe
      fitnessCertExpiry UTCTime Maybe
      permitNumber Text Maybe
      permitStart UTCTime Maybe
      permitExpiry UTCTime Maybe
      vehicleClass Domain.VehicleClass Maybe
      vehicleRegStatus Domain.Verification2
      vehicleNumber Text Maybe
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
          ..
        }
  toTType Domain.VehicleRegistrationCert {..} =
    VehicleRegistrationCertT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }
  



    