module SharedLogic.MetroOffer
  ( MetroOffer (..),
    MetroRide (..),
    ScheduleElement (..),
    MetroStation (..),
    cacheMetroOffers,
    getMetroOffers,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.SearchRequest (SearchRequest)

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
  fromMaybe [] <$> Redis.get (metroOfferKey searchReqId)

metroOfferKey :: Id SearchRequest -> Text
metroOfferKey (Id id') = "BAP:Metro:" <> id'
