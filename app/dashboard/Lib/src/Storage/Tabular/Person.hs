{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
      emailEncrypted Text
      emailHash DbHash
      mobileNumberEncrypted Text
      mobileNumberHash DbHash
      mobileCountryCode Text
      passwordHash DbHash
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey (Id id) = PersonTKey id

instance TType PersonT Domain.Person where
  fromTType PersonT {..} = do
    return $
      Domain.Person
        { id = Id id,
          roleId = fromKey roleId,
          email = EncryptedHashed (Encrypted emailEncrypted) emailHash,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        roleId = toKey roleId,
        emailEncrypted = email & unEncrypted . (.encrypted),
        emailHash = email.hash,
        mobileNumberEncrypted = mobileNumber & unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber.hash,
        ..
      }
