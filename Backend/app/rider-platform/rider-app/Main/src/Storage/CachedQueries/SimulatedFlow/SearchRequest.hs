{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.SimulatedFlow.SearchRequest where

import Data.Text
import Domain.Types.Booking as DB
import Domain.Types.Estimate as DEstimate
import Domain.Types.Person as DP
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Tools.Maps as Maps

cacheSearchRequest :: SimluatedCacheFlow m r => SearchRequest -> Person -> m ()
cacheSearchRequest searchRequest person = do
  let pointKey = makeIdKey searchRequest.id
  let listKey = makeListIdKey person.id
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.lPush listKey (searchRequest.id.getId :| [])
  Hedis.expire listKey expTime
  Hedis.setExp pointKey searchRequest expTime

cacheRouteInfo :: SimluatedCacheFlow m r => Id SearchRequest -> Maps.RouteInfo -> m ()
cacheRouteInfo searchReqId shortestRouteInfo = do
  let pointKey = mkRouteKey searchReqId
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.setExp pointKey shortestRouteInfo expTime

cacheByEstimateId :: SimluatedCacheFlow m r => Id DEstimate.Estimate -> DEstimate.EstimateAPIEntity -> SearchRequest -> m ()
cacheByEstimateId estimateId estimate searchReq = do
  let pointKey = mkEstimatePointKey estimateId
  let searchLinkKey = mkEstimateSearchLinkKey estimateId
  let personLinkKey = mkEstimateRiderLinkKey estimateId
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.setExp pointKey estimate expTime
  Hedis.setExp searchLinkKey searchReq.id.getId expTime
  Hedis.setExp personLinkKey searchReq.riderId.getId expTime

linkBookingWithPerson :: SimluatedCacheFlow m r => Id DB.Booking -> Id DP.Person -> m ()
linkBookingWithPerson bookingId personId = do
  let bookingLinkKey = mkBookingPersonLinkKey personId
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.setExp bookingLinkKey bookingId.getId expTime

getBookingIdByPersonId :: SimluatedCacheFlow m r => Id DP.Person -> m (Maybe (Id DB.Booking))
getBookingIdByPersonId personId = fmap Id <$> Hedis.get (mkBookingPersonLinkKey personId)

mkBookingPersonLinkKey :: Id DP.Person -> Text
mkBookingPersonLinkKey personId = "CachedQueries:SimulatedUser:Booking:PersonLink" <> personId.getId

linkBookingToQuoteId :: SimluatedCacheFlow m r => Id DQ.Quote -> Id DB.Booking -> m ()
linkBookingToQuoteId quoteId bookingId = do
  let quoteLinkKey = mkQuoteBookingLinkKey bookingId
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.setExp quoteLinkKey quoteId.getId expTime

getQuoteIdByBookingId :: SimluatedCacheFlow m r => Id DB.Booking -> m (Maybe (Id DQ.Quote))
getQuoteIdByBookingId bookingId = fmap Id <$> Hedis.get (mkQuoteBookingLinkKey bookingId)

linkEstimateIdWithQuoteId :: SimluatedCacheFlow m r => Id DEstimate.Estimate -> Id DQ.Quote -> m ()
linkEstimateIdWithQuoteId estimateId quoteId = do
  let quoteLinkKey = mkEstimateQuoteLinkKey quoteId
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Hedis.setExp quoteLinkKey estimateId.getId expTime

mkQuoteBookingLinkKey :: Id DB.Booking -> Text
mkQuoteBookingLinkKey quoteId = "CachedQueries:SimulatedUser:Quote:BookingLink" <> quoteId.getId

getEstimateIdByQuoteId :: SimluatedCacheFlow m r => Id DQ.Quote -> m (Maybe (Id DEstimate.Estimate))
getEstimateIdByQuoteId quoteId = fmap Id <$> Hedis.get (mkEstimateQuoteLinkKey quoteId)

getEstimateById :: SimluatedCacheFlow m r => Id DEstimate.Estimate -> m (Maybe DEstimate.EstimateAPIEntity)
getEstimateById estimateId = Hedis.get $ mkEstimatePointKey estimateId

getRouteInfoBySearchReqId :: SimluatedCacheFlow m r => Id SearchRequest -> m (Maybe Maps.RouteInfo)
getRouteInfoBySearchReqId searchReqId = Hedis.get $ mkRouteKey searchReqId

getSearchRequestById :: SimluatedCacheFlow m r => Id SearchRequest -> m (Maybe SearchRequest)
getSearchRequestById searchReqId = Hedis.get $ makeIdKey searchReqId

getRiderIdByEstimateId :: SimluatedCacheFlow m r => Id DEstimate.Estimate -> m (Maybe (Id Person))
getRiderIdByEstimateId estimateId = fmap Id <$> Hedis.get (mkEstimateRiderLinkKey estimateId)

getSearchRequestIdByEstimateId :: SimluatedCacheFlow m r => Id DEstimate.Estimate -> m (Maybe (Id SearchRequest))
getSearchRequestIdByEstimateId estimateId = fmap Id <$> Hedis.get (mkEstimateSearchLinkKey estimateId)

mkEstimateQuoteLinkKey :: Id DQ.Quote -> Text
mkEstimateQuoteLinkKey estimateId = "CachedQueries:SimulatedUser:Estimate:QuoteLink" <> estimateId.getId

mkEstimateRiderLinkKey :: Id a -> Text
mkEstimateRiderLinkKey estimateId = "CachedQueries:SimulatedUser:Estimate:RiderLink" <> estimateId.getId

mkEstimateSearchLinkKey :: Id a -> Text
mkEstimateSearchLinkKey estimateId = "CachedQueries:SimulatedUser:Estimate:SearchLink" <> estimateId.getId

mkEstimatePointKey :: Id a -> Text
mkEstimatePointKey estimateId = "CachedQueries:SimulatedUser:Estimate:Fare" <> estimateId.getId

mkRouteKey :: Id SearchRequest -> Text
mkRouteKey searchReqId = "CachedQueries:SimulatedUser:SearchRequest:Route:" <> searchReqId.getId

makeListIdKey :: Id Person -> Text
makeListIdKey personId = "CachedQueries:SimulatedUser:PersonId:AllSearchRequests:" <> personId.getId

makeIdKey :: Id SearchRequest -> Text
makeIdKey searchReqId = "CachedQueries:SimulatedUser:SearchRequest:" <> searchReqId.getId
