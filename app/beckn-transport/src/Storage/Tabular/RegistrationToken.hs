{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RegistrationToken where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.App (RegToken)
import Beckn.Types.Id
import qualified Domain.Types.RegistrationToken as Domain

derivePersistField "Domain.Medium"
derivePersistField "Domain.RTEntityType"
derivePersistField "Domain.LoginType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RegistrationTokenT sql=registration_token
      id Text
      token RegToken
      attempts Int
      authMedium Domain.Medium
      authType Domain.LoginType
      authValueHash Text
      verified Bool
      authExpiry Int
      tokenExpiry Int
      entityId Text
      entityType Domain.RTEntityType
      createdAt UTCTime
      updatedAt UTCTime
      info Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RegistrationTokenT where
  type DomainKey RegistrationTokenT = Id Domain.RegistrationToken
  fromKey (RegistrationTokenTKey _id) = Id _id
  toKey (Id id) = RegistrationTokenTKey id

instance TType RegistrationTokenT Domain.RegistrationToken where
  fromTType RegistrationTokenT {..} = do
    return $
      Domain.RegistrationToken
        { id = Id id,
          ..
        }
  toTType Domain.RegistrationToken {..} =
    RegistrationTokenT
      { id = getId id,
        ..
      }
