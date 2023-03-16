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

module Storage.Tabular.Person.PersonDefaultEmergencyNumber where

import qualified Domain.Types.Person as Domain
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as Domain
import Kernel.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonDefaultEmergencyNumberT sql=person_default_emergency_number
      personId PersonTId
      name Text
      mobileCountryCode Text
      mobileNumberEncrypted Text
      mobileNumberHash DbHash
      createdAt UTCTime
      Primary personId
      UniquePersonDefaultEmergencyNumberPersonIdAndMobileNumber personId mobileCountryCode mobileNumberHash
      deriving Generic
    |]

instance TEntityKey PersonDefaultEmergencyNumberT where
  type DomainKey PersonDefaultEmergencyNumberT = Id Domain.Person
  fromKey (PersonDefaultEmergencyNumberTKey _id) = fromKey _id
  toKey id = PersonDefaultEmergencyNumberTKey $ toKey id

instance FromTType PersonDefaultEmergencyNumberT Domain.PersonDefaultEmergencyNumber where
  fromTType PersonDefaultEmergencyNumberT {..} = do
    return $
      Domain.PersonDefaultEmergencyNumber
        { personId = fromKey personId,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }

instance ToTType PersonDefaultEmergencyNumberT Domain.PersonDefaultEmergencyNumber where
  toTType Domain.PersonDefaultEmergencyNumber {..} =
    PersonDefaultEmergencyNumberT
      { personId = toKey personId,
        mobileNumberEncrypted = unEncrypted (mobileNumber.encrypted),
        mobileNumberHash = mobileNumber.hash,
        ..
      }
