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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import qualified Domain.Types.Quote as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMeters)
import Kernel.Types.Id
import Storage.Tabular.Search (SearchTId)
import Storage.Tabular.TransportStation (TransportStationTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      searchId SearchTId
      bppId Text
      bppUrl Text
      description Text
      fare HighPrecMeters
      departureTime UTCTime
      arrivalTime UTCTime
      departureStationId TransportStationTId
      arrivalStationId TransportStationTId
      createdAt UTCTime
      routeCode Text
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey id = QuoteTKey id.getId

instance FromTType QuoteT Domain.Quote where
  fromTType QuoteT {..} = do
    bppUrl_ <- parseBaseUrl bppUrl
    return $
      Domain.Quote
        { id = Id id,
          searchId = fromKey searchId,
          bppUrl = bppUrl_,
          departureStationId = fromKey departureStationId,
          arrivalStationId = fromKey arrivalStationId,
          fare = roundToIntegral fare,
          ..
        }

instance ToTType QuoteT Domain.Quote where
  toTType Domain.Quote {..} = do
    QuoteT
      { id = id.getId,
        searchId = toKey searchId,
        bppUrl = showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        fare = realToFrac fare,
        ..
      }
