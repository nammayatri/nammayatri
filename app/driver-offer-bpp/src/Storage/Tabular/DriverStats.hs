{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverStats where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverStatsT sql=driver_stats
      driverId PersonTId
      idleSince UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverStatsT where
  type DomainKey DriverStatsT = Id Driver
  fromKey (DriverStatsTKey _id) = cast $ fromKey _id
  toKey id = DriverStatsTKey . toKey $ cast id

instance TType DriverStatsT Domain.DriverStats where
  fromTType DriverStatsT {..} = do
    return $
      Domain.DriverStats
        { driverId = cast $ fromKey driverId,
          ..
        }
  toTType Domain.DriverStats {..} =
    DriverStatsT
      { driverId = toKey . cast $ driverId,
        ..
      }
