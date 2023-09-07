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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Person where

import qualified Domain.Types.Person as Domain
import Kernel.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Role (RoleTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      firstName Text
      lastName Text
      roleId RoleTId
      emailEncrypted Text Maybe
      emailHash DbHash Maybe
      mobileNumberEncrypted Text
      mobileNumberHash DbHash
      mobileCountryCode Text
      passwordHash DbHash Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic

    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey (Id id) = PersonTKey id

instance FromTType PersonT Domain.Person where
  fromTType PersonT {..} = do
    return $
      Domain.Person
        { id = Id id,
          roleId = fromKey roleId,
          email = case (emailEncrypted, emailHash) of
            (Just email, Just hash) -> Just $ EncryptedHashed (Encrypted email) hash
            _ -> Nothing,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }

instance ToTType PersonT Domain.Person where
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        roleId = toKey roleId,
        emailEncrypted = email <&> (unEncrypted . (.encrypted)),
        emailHash = email <&> (.hash),
        mobileNumberEncrypted = mobileNumber & unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber.hash,
        ..
      }
