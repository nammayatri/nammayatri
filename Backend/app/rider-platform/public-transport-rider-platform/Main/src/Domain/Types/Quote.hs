 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Quote where

import Domain.Types.Search
import Domain.Types.TransportStation
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty (PrettyShow)

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Money,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id TransportStation,
    arrivalStationId :: Id TransportStation,
    createdAt :: UTCTime,
    routeCode :: Text
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    description :: Text,
    fare :: Money,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: TransportStationAPIEntity,
    arrivalStation :: TransportStationAPIEntity,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, PrettyShow, Show)

makeQuoteAPIEntity :: Quote -> TransportStation -> TransportStation -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} departureStation arrivalStation = do
  let departureStationAPIEntity = makeTransportStationAPIEntity departureStation
      arrivalStationAPIEntity = makeTransportStationAPIEntity arrivalStation
  QuoteAPIEntity
    { departureStation = departureStationAPIEntity,
      arrivalStation = arrivalStationAPIEntity,
      ..
    }
