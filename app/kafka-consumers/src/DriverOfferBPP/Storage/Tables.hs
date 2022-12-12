{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DriverOfferBPP.Storage.Tables where

import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Time
import qualified DriverOfferBPP.Types as Domain
import EulerHS.Prelude hiding (id)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverAvailabilityT sql=driver_availability
      id Text
      driverId Text
      merchantId Text

      totalAvailableTime Int
      lastAvailableTime UTCTime
      bucketStartTime UTCTime
      bucketEndTime UTCTime
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey DriverAvailabilityT where
  type DomainKey DriverAvailabilityT = Id Domain.DriverAvailability
  fromKey (DriverAvailabilityTKey _id) = Id _id
  toKey (Id id) = DriverAvailabilityTKey id

instance TType DriverAvailabilityT Domain.DriverAvailability where
  fromTType DriverAvailabilityT {..} =
    return
      Domain.DriverAvailability
        { id = Id id,
          ..
        }
  toTType Domain.DriverAvailability {..} =
    DriverAvailabilityT
      { id = getId id,
        ..
      }
