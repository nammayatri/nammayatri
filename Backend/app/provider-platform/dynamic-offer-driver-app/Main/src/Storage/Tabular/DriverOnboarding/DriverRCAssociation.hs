{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

instance FromTType DriverRCAssociationT Domain.DriverRCAssociation where
  fromTType DriverRCAssociationT {..} = do
    return $
      Domain.DriverRCAssociation
        { id = Id id,
          driverId = fromKey driverId,
          rcId = fromKey rcId,
          ..
        }

instance ToTType DriverRCAssociationT Domain.DriverRCAssociation where
  toTType Domain.DriverRCAssociation {..} =
    DriverRCAssociationT
      { id = getId id,
        driverId = toKey driverId,
        rcId = toKey rcId,
        ..
      }
