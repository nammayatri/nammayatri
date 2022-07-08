{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driveronboarding.OperatingCity where
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Storage.Tabular.Organization (OrganizationTId)
import qualified Domain.Types.Driveronboarding.OperatingCity as Domain
import Beckn.Types.Id


mkPersist
  defaultSqlSettings
  [defaultQQ|
    OperatingCityT sql = operating_city
      id Text
      organizationId OrganizationTId
      cityName Text
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]   
instance TEntityKey OperatingCityT where
  type DomainKey OperatingCityT = Id Domain.OperatingCity
  fromKey (OperatingCityTKey _id) = Id _id
  toKey (Id id) = OperatingCityTKey id

instance TType OperatingCityT Domain.OperatingCity where
  fromTType OperatingCityT {..} = do
    return $
      Domain.OperatingCity
        { id = Id id,
          organizationId = fromKey organizationId,
          ..
        }
   
  toTType Domain.OperatingCity {..} =
    OperatingCityT
      { id = getId id,
        organizationId = toKey organizationId,
        ..
      }
