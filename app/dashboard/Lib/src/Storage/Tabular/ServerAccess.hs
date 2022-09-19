{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.ServerAccess where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RegistrationToken as Domain
import qualified Domain.Types.ServerAccess as Domain
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.ServerName"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ServerAccessT sql=server_access
      id Text
      personId PersonTId
      serverName Domain.ServerName
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ServerAccessT where
  type DomainKey ServerAccessT = Id Domain.ServerAccess
  fromKey (ServerAccessTKey _id) = Id _id
  toKey (Id id) = ServerAccessTKey id

instance TType ServerAccessT Domain.ServerAccess where
  fromTType ServerAccessT {..} = do
    return $
      Domain.ServerAccess
        { id = Id id,
          personId = fromKey personId,
          ..
        }
  toTType Domain.ServerAccess {..} =
    ServerAccessT
      { id = getId id,
        personId = toKey personId,
        ..
      }
