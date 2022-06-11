{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TransporterConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.TransporterConfig as Domain
import Storage.Tabular.Organization (OrganizationTId)

derivePersistField "Domain.ConfigKey"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransporterConfigT sql=transporter_config
      id Text
      transporterId OrganizationTId
      configKey Domain.ConfigKey sql=key
      value Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey TransporterConfigT where
  type DomainKey TransporterConfigT = Id Domain.TransporterParameter
  fromKey (TransporterConfigTKey _id) = Id _id
  toKey (Id id) = TransporterConfigTKey id

instance TEntity TransporterConfigT Domain.TransporterConfig where
  fromTEntity entity = do
    let TransporterConfigT {..} = entityVal entity
    return $
      Domain.TransporterConfig
        { id = Id id,
          transporterId = fromKey transporterId,
          key = configKey,
          ..
        }
  toTType Domain.TransporterConfig {..} =
    TransporterConfigT
      { id = getId id,
        transporterId = toKey transporterId,
        configKey = key,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
