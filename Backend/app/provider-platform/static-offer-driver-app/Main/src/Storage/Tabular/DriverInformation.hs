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

module Storage.Tabular.DriverInformation where

import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      adminId PersonTId Maybe
      active Bool
      onRide Bool
      enabled Bool
      blocked Bool
      optForRental Bool
      createdAt UTCTime
      updatedAt UTCTime
      canDowngradeToSedan Bool
      canDowngradeToHatchback Bool
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance FromTType DriverInformationT Domain.DriverInformation where
  fromTType DriverInformationT {..} = do
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          adminId = fromKey <$> adminId,
          ..
        }

instance ToTType DriverInformationT Domain.DriverInformation where
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        adminId = toKey <$> adminId,
        ..
      }
