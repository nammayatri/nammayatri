{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequestForDriver where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Meters, Money)
import Beckn.Types.Id
import Beckn.Types.Time
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.DriverSearchRequestStatus"
derivePersistField "Domain.SearchRequestForDriverResponse"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestForDriverT sql=search_request_for_driver
      id Text
      searchRequestId SearchRequestTId
      startTime UTCTime
      actualDistanceToPickup Meters
      straightLineDistanceToPickup Meters
      durationToPickup Seconds
      vehicleVariant Variant.Variant
      batchNumber Int
      baseFare Money
      lat Double Maybe
      lon Double Maybe
      searchRequestValidTill UTCTime
      driverId PersonTId
      status Domain.DriverSearchRequestStatus
      createdAt UTCTime
      response Domain.SearchRequestForDriverResponse Maybe
      driverMinExtraFee Money
      driverMaxExtraFee Money
      rideRequestPopupDelayDuration Seconds
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestForDriverT where
  type DomainKey SearchRequestForDriverT = Id Domain.SearchRequestForDriver
  fromKey (SearchRequestForDriverTKey _id) = Id _id
  toKey (Id id) = SearchRequestForDriverTKey id

instance TType SearchRequestForDriverT Domain.SearchRequestForDriver where
  fromTType SearchRequestForDriverT {..} = do
    return $
      Domain.SearchRequestForDriver
        { id = Id id,
          driverId = fromKey driverId,
          searchRequestId = fromKey searchRequestId,
          ..
        }
  toTType Domain.SearchRequestForDriver {..} =
    SearchRequestForDriverT
      { id = getId id,
        driverId = toKey driverId,
        searchRequestId = toKey searchRequestId,
        ..
      }
