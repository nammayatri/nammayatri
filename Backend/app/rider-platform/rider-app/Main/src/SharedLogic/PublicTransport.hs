{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PublicTransport
  ( sendPublicTransportSearchRequest,
    getPublicTransportOffers,
    cachePublicTransportOffers,
  )
where

import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch
import Kernel.Streaming.MonadProducer
import Kernel.Types.Id
import Kernel.Utils.Common

sendPublicTransportSearchRequest ::
  MonadProducer PublicTransportSearch m =>
  Id Person.Person ->
  DSearch.SearchRes ->
  m ()
sendPublicTransportSearchRequest personId DSearch.SearchRes {..} = do
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
