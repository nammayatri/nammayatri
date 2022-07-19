{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequestForDriver where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Meters (..))
import Beckn.Types.Id
import Beckn.Types.Time
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestForDriverT sql=search_request_for_driver
      id Text
      searchRequestId SearchRequestTId
      messageId Text
      startTime UTCTime
      distanceToPickup Meters
      durationToPickup Seconds
      vehicleVariant Variant.Variant
      distance Double
      baseFare Double
      searchRequestValidTill UTCTime
      driverId PersonTId
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

deriving newtype instance PersistField Seconds

deriving newtype instance PersistField Meters

deriving newtype instance PersistFieldSql Seconds

deriving newtype instance PersistFieldSql Meters

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
