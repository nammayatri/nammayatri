{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.DriverRCAssociation where

import qualified Domain.Types.DriverOnboarding.DriverRCAssociation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate (VehicleRegistrationCertificateTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverRCAssociationT sql=driver_rc_association
      id Text
      driverId PersonTId
      rcId VehicleRegistrationCertificateTId
      associatedOn UTCTime
      associatedTill UTCTime Maybe
      consent Bool
      consentTimestamp UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverRCAssociationT where
  type DomainKey DriverRCAssociationT = Id Domain.DriverRCAssociation
  fromKey (DriverRCAssociationTKey _id) = Id _id
  toKey (Id id) = DriverRCAssociationTKey id

instance TType DriverRCAssociationT Domain.DriverRCAssociation where
  fromTType DriverRCAssociationT {..} = do
    return $
      Domain.DriverRCAssociation
        { id = Id id,
          driverId = fromKey driverId,
          rcId = fromKey rcId,
          ..
        }

  toTType Domain.DriverRCAssociation {..} =
    DriverRCAssociationT
      { id = getId id,
        driverId = toKey driverId,
        rcId = toKey rcId,
        ..
      }
