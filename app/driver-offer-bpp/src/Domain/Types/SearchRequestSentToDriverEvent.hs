module Domain.Types.SearchRequestSentToDriverEvent where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.SearchRequest

data SearchRequestSentToDriverEvent = SearchRequestSentToDriverEvent
  { id :: Id SearchRequestSentToDriverEvent,
    searchRequestId :: Id SearchRequest,
    searchRequestValidTill :: UTCTime,
    driverId :: Id Person,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    baseFare :: Double,
    createdAt :: UTCTime
  }
