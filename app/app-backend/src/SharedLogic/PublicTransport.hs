module SharedLogic.PublicTransport
  ( sendPublicTransportSearchRequest,
    getPublicTransportOffers,
    cachePublicTransportOffers,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadProducer
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSR

sendPublicTransportSearchRequest ::
  MonadProducer PublicTransportSearch m =>
  Id Person.Person ->
  DOneWaySearch.OneWaySearchRes ->
  m ()
sendPublicTransportSearchRequest personId DOneWaySearch.OneWaySearchRes {..} = do
  producePublicTransportSearchMessage publicTransportSearch
  where
    publicTransportSearch =
      PublicTransportSearch
        { id = getId searchId,
          gps = origin.gps,
          requestorId = getId personId,
          createdAt = now
        }

getPublicTransportOffers :: (HedisFlow m r, MonadFlow m) => Id DSR.SearchRequest -> m [PublicTransportQuote]
getPublicTransportOffers transactionId =
  Hedis.getList $ makeRedisKey transactionId

cachePublicTransportOffers ::
  (HedisFlow m r, ToJSON a) =>
  Id DSR.SearchRequest ->
  [a] ->
  m ()
cachePublicTransportOffers transactionId quoteList =
  Hedis.rPushExp (makeRedisKey transactionId) quoteList expirationTime
  where
    expirationTime = 600

makeRedisKey :: Id DSR.SearchRequest -> Text
makeRedisKey transactionId = "publicTransportQuoteList:" <> getId transactionId
