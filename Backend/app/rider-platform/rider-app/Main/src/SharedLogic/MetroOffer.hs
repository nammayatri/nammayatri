{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MetroOffer
  ( MetroOffer (..),
    MetroRide (..),
    ScheduleElement (..),
    MetroStation (..),
    cacheMetroOffers,
    getMetroOffers,
  )
where

import Domain.Types.SearchRequest (SearchRequest)
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id

data MetroOffer = MetroOffer
  { rideSearchId :: Id SearchRequest, -- search case id now
    description :: Text,
    rides :: [MetroRide],
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data MetroRide = MetroRide
  { schedule :: [ScheduleElement],
    departureStation :: MetroStation,
    arrivalStation :: MetroStation,
    price :: Money
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data ScheduleElement = ScheduleElement
  { departureTime :: UTCTime,
    arrivalTime :: UTCTime
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data MetroStation = MetroStation
  { name :: Text,
    stationCode :: Maybe Text,
    point :: LatLong
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

cacheMetroOffers ::
  (Redis.HedisFlow m r, MonadTime m) =>
  MonadFlow m =>
  Id SearchRequest ->
  [MetroOffer] ->
  m ()
cacheMetroOffers searchReqId offers =
  Redis.setExp (metroOfferKey searchReqId) offers (60 * 60 * 24)

getMetroOffers ::
  ( Redis.HedisFlow m r,
    FromJSON a
  ) =>
  Id SearchRequest ->
  m [a]
getMetroOffers searchReqId =
  fold <$> Redis.get (metroOfferKey searchReqId)

metroOfferKey :: Id SearchRequest -> Text
metroOfferKey (Id id') = "BAP:Metro:" <> id'
