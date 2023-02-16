{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Person (PersonTId)
import qualified Storage.Tabular.SearchRequest as SReq
import qualified Storage.Tabular.SearchRequestForDriver as SRFD
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.DriverQuoteStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverQuoteT sql=driver_quote
      id Text
      status Domain.DriverQuoteStatus
      searchRequestId SReq.SearchRequestTId
      searchRequestForDriverId SRFD.SearchRequestForDriverTId Maybe
      driverId PersonTId
      driverName Text
      driverRating Centesimal Maybe
      vehicleVariant Variant.Variant
      distance Meters
      distanceToPickup Meters
      durationToPickup Double
      validTill UTCTime
      estimatedFare Common.Money
      fareParametersId Fare.FareParametersTId
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey DriverQuoteT where
  type DomainKey DriverQuoteT = Id Domain.DriverQuote
  fromKey (DriverQuoteTKey _id) = Id _id
  toKey (Id id) = DriverQuoteTKey id

instance TType (DriverQuoteT, Fare.FareParametersT) Domain.DriverQuote where
  fromTType (DriverQuoteT {..}, fareParamsT) = do
    fareParams <- fromTType fareParamsT
    return $
      Domain.DriverQuote
        { id = Id id,
          searchRequestId = fromKey searchRequestId,
          searchRequestForDriverId = fromKey <$> searchRequestForDriverId,
          driverId = fromKey driverId,
          durationToPickup = roundToIntegral durationToPickup,
          ..
        }
  toTType Domain.DriverQuote {..} =
    ( DriverQuoteT
        { id = getId id,
          searchRequestId = toKey searchRequestId,
          searchRequestForDriverId = toKey <$> searchRequestForDriverId,
          driverId = toKey driverId,
          durationToPickup = realToFrac durationToPickup,
          fareParametersId = toKey fareParams.id,
          ..
        },
      toTType fareParams
    )
