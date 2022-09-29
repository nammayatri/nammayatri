{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TransporterConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Meters)
import Beckn.Types.Id
import qualified Domain.Types.Organization as Domain
import qualified Domain.Types.TransporterConfig as Domain
import Storage.Tabular.Organization (OrganizationTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransporterConfigT sql=transporter_config
      organizationId OrganizationTId
      pickupLocThreshold Meters Maybe
      dropLocThreshold Meters Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary organizationId
      deriving Generic
    |]

instance TEntityKey TransporterConfigT where
  type DomainKey TransporterConfigT = Id Domain.Organization
  fromKey (TransporterConfigTKey _id) = fromKey _id
  toKey id = TransporterConfigTKey $ toKey id

instance TType TransporterConfigT Domain.TransporterConfig where
  fromTType TransporterConfigT {..} = do
    return $
      Domain.TransporterConfig
        { organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.TransporterConfig {..} =
    TransporterConfigT
      { organizationId = toKey organizationId,
        ..
      }
