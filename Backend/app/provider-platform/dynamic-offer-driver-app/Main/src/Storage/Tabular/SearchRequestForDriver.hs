{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequestForDriver where

import qualified Domain.Types.DriverInformation as D
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.SearchTry (SearchTryTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.DriverSearchRequestStatus"
derivePersistField "Domain.SearchRequestForDriverResponse"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestForDriverT sql=search_request_for_driver
      id Text
      requestId SearchRequestTId sql=search_request_id
      searchTryId SearchTryTId
      startTime UTCTime
      actualDistanceToPickup Meters
      straightLineDistanceToPickup Meters
      durationToPickup Seconds
      vehicleVariant Variant.Variant
      batchNumber Int
      lat Double Maybe
      lon Double Maybe
      searchRequestValidTill UTCTime
      driverId PersonTId
      status Domain.DriverSearchRequestStatus
      response Domain.SearchRequestForDriverResponse Maybe
      driverMinExtraFee Money Maybe
      driverMaxExtraFee Money Maybe
      rideRequestPopupDelayDuration Seconds
      isPartOfIntelligentPool Bool
      cancellationRatio Double Maybe
      acceptanceRatio Double Maybe
      driverAvailableTime Double Maybe
      parallelSearchRequestCount Int Maybe
      driverSpeed Double Maybe
      keepHiddenForSeconds Seconds
      mode D.DriverMode Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestForDriverT where
  type DomainKey SearchRequestForDriverT = Id Domain.SearchRequestForDriver
  fromKey (SearchRequestForDriverTKey _id) = Id _id
  toKey (Id id) = SearchRequestForDriverTKey id

instance FromTType SearchRequestForDriverT Domain.SearchRequestForDriver where
  fromTType SearchRequestForDriverT {..} = do
    return $
      Domain.SearchRequestForDriver
        { id = Id id,
          driverId = fromKey driverId,
          requestId = fromKey requestId,
          searchTryId = fromKey searchTryId,
          ..
        }

instance ToTType SearchRequestForDriverT Domain.SearchRequestForDriver where
  toTType :: Domain.SearchRequestForDriver -> SearchRequestForDriverT
  toTType Domain.SearchRequestForDriver {..} =
    SearchRequestForDriverT
      { id = getId id,
        driverId = toKey driverId,
        requestId = toKey requestId,
        searchTryId = toKey searchTryId,
        ..
      }
