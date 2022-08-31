{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Person where

import Beckn.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Person as Domain

derivePersistField "Domain.Role"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      firstName Text Maybe
      lastName Text Maybe
      role Domain.Role
      emailEncrypted Text Maybe
      emailHash DbHash Maybe
      mobileNumberEncrypted Text Maybe
      mobileNumberHash DbHash Maybe
      mobileCountryCode Text Maybe
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

instance TType PersonT Domain.Person where
  fromTType PersonT {..} = do
    return $
      Domain.Person
        { id = Id id,
          email = EncryptedHashed <$> (Encrypted <$> emailEncrypted) <*> emailHash,
          mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
          ..
        }
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        emailEncrypted = email <&> unEncrypted . (.encrypted),
        emailHash = email <&> (.hash),
        mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber <&> (.hash),
        ..
      }
