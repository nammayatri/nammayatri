{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount (Amount)
import Beckn.Types.Common (Meters (..), Seconds (..))
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Person (PersonTId)
import qualified Storage.Tabular.SearchRequest as SReq
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.DriverQuoteStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverQuoteT sql=driver_quote
      id Text
      status Domain.DriverQuoteStatus
      searchRequestId SReq.SearchRequestTId
      driverId PersonTId
      driverName Text
      driverRating Double Maybe
      vehicleVariant Variant.Variant
      distance Double
      distanceToPickup Int
      durationToPickup Double
      validTill UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      estimatedFare Amount
      fareParametersId Fare.FareParametersTId

      Primary id
      deriving Generic
    |]

instance TEntityKey DriverQuoteT where
  type DomainKey DriverQuoteT = Id Domain.DriverQuote
  fromKey (DriverQuoteTKey _id) = Id _id
  toKey (Id id) = DriverQuoteTKey id

instance TType (DriverQuoteT, Fare.FareParametersT) Domain.DriverQuote where
  fromTType (DriverQuoteT {..}, fareParams) = do
    return $
      Domain.DriverQuote
        { id = Id id,
          searchRequestId = fromKey searchRequestId,
          driverId = fromKey driverId,
          distanceToPickup = Meters distanceToPickup,
          durationToPickup = Seconds $ floor durationToPickup,
          fareParams = Fare.mkDomainFromTabularFareParams fareParams,
          ..
        }
  toTType Domain.DriverQuote {..} =
    let fareParamsId = cast id
     in ( DriverQuoteT
            { id = getId id,
              searchRequestId = toKey searchRequestId,
              driverId = toKey driverId,
              distanceToPickup = getMeters distanceToPickup,
              durationToPickup = fromIntegral $ getSeconds durationToPickup,
              fareParametersId = toKey fareParamsId,
              ..
            },
          Fare.mkTabularFromDomainFareParams fareParamsId fareParams
        )
