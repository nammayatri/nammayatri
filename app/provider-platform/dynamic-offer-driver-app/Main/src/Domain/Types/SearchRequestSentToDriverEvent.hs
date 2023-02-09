module Domain.Types.SearchRequestSentToDriverEvent where

import Domain.Types.Person
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

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
