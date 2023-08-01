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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverQuote where

import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Centesimal, Meters (..))
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Storage.Tabular.Estimate (EstimateTId)
import qualified Storage.Tabular.FareParameters as Fare
import qualified Storage.Tabular.FareParameters.Instances as Fare
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import qualified Storage.Tabular.SearchRequestForDriver as SRFD
import Storage.Tabular.SearchTry (SearchTryTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.DriverQuoteStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverQuoteT sql=driver_quote
      id Text
      requestId SearchRequestTId sql=search_request_id
      searchTryId SearchTryTId
      searchRequestForDriverId SRFD.SearchRequestForDriverTId Maybe
      estimateId EstimateTId
      driverId PersonTId
      driverName Text
      driverRating Centesimal Maybe
      status Domain.DriverQuoteStatus
      vehicleVariant Variant.Variant
      distance Meters
      distanceToPickup Meters
      durationToPickup Double
      validTill UTCTime
      estimatedFare Common.Money
      fareParametersId Fare.FareParametersTId
      providerId MerchantTId
      specialLocationTag Text Maybe
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey DriverQuoteT where
  type DomainKey DriverQuoteT = Id Domain.DriverQuote
  fromKey (DriverQuoteTKey _id) = Id _id
  toKey (Id id) = DriverQuoteTKey id

type FullDriverQuoteT = (DriverQuoteT, Fare.FullFareParametersT)

instance FromTType FullDriverQuoteT Domain.DriverQuote where
  fromTType (DriverQuoteT {..}, fareParamsT) = do
    fareParams <- fromTType fareParamsT
    return $
      Domain.DriverQuote
        { id = Id id,
          requestId = fromKey requestId,
          searchTryId = fromKey searchTryId,
          searchRequestForDriverId = fromKey <$> searchRequestForDriverId,
          driverId = fromKey driverId,
          durationToPickup = roundToIntegral durationToPickup,
          providerId = fromKey providerId,
          estimateId = fromKey estimateId,
          ..
        }

instance ToTType FullDriverQuoteT Domain.DriverQuote where
  toTType Domain.DriverQuote {..} =
    ( DriverQuoteT
        { id = getId id,
          requestId = toKey requestId,
          searchTryId = toKey searchTryId,
          searchRequestForDriverId = toKey <$> searchRequestForDriverId,
          driverId = toKey driverId,
          durationToPickup = realToFrac durationToPickup,
          fareParametersId = toKey fareParams.id,
          providerId = toKey providerId,
          estimateId = toKey estimateId,
          ..
        },
      toTType fareParams
    )
