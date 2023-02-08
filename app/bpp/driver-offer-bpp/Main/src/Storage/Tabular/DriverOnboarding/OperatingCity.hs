{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.OperatingCity where

import qualified Domain.Types.DriverOnboarding.OperatingCity as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OperatingCityT sql=operating_city
      id Text
      merchantId MerchantTId
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
          merchantId = fromKey merchantId,
          ..
        }

  toTType Domain.OperatingCity {..} =
    OperatingCityT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
