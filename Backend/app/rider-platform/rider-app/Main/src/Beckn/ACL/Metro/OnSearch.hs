 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Metro.OnSearch where

import qualified Beckn.Types.Core.Metro.API.OnSearch as OnSearch
import Beckn.Types.Core.Metro.OnSearch
import qualified Data.List.NonEmpty as NE
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MetroOffer

buildMetroOffers ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnSearch.OnSearchReq ->
  m (Maybe [MetroOffer])
buildMetroOffers req = do
  validateMetroContext Context.ON_SEARCH req.context
  let searchReqId = Id req.context.message_id
  handleError req.contents $ \message -> do
    catalogToMetroOffers searchReqId message.catalog

handleError ::
  (MonadFlow m) =>
  Either Error OnSearch.OnSearchCatalog ->
  (OnSearch.OnSearchCatalog -> m [MetroOffer]) ->
  m (Maybe [MetroOffer])
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

catalogToMetroOffers :: (MonadThrow m, Log m, MonadTime m) => Id SearchRequest -> Catalog -> m [MetroOffer]
catalogToMetroOffers searchRequestId Catalog {bpp_providers} =
  traverse (providerToMetroOffer searchRequestId) bpp_providers

providerToMetroOffer :: (MonadThrow m, Log m, MonadTime m) => Id SearchRequest -> Provider -> m MetroOffer
providerToMetroOffer rideSearchId Provider {descriptor, items, fulfillments} = do
  description <- descriptor.name & fromMaybeM (InvalidRequest "Provider is missing descriptor.name")
  offerInfos <- uniteItemAndFulfillment items fulfillments
  rides <- offerInfoToMetroRide `traverse` offerInfos
  createdAt <- getCurrentTime
  return $ MetroOffer {..}

uniteItemAndFulfillment :: (MonadThrow m, Log m) => [Item] -> [Fulfillment] -> m [(Item, NonEmpty Fulfillment)]
uniteItemAndFulfillment items fulfillments = do
  items `for` \item -> do
    itemFulfillments <- findFulfillments item.fulfillment_id
    pure (item, itemFulfillments)
  where
    findFulfillments id = do
      let filteredFulfillments = filter (\fulfillment -> fulfillment.id == id) fulfillments
      case filteredFulfillments of
        [] -> throwError $ InvalidRequest $ "Fulfillment " <> id <> " not found in provider.fulfillments"
        (f : fs) -> pure $ f :| fs

offerInfoToMetroRide :: (MonadTime m, MonadThrow m, Log m) => (Item, NonEmpty Fulfillment) -> m MetroRide
offerInfoToMetroRide (item, fulfillments) = do
  price <-
    roundToIntegral <$> item.price.value
      & fromMaybeM (InvalidRequest "Missing price.value in item")
  now <- getCurrentTime
  let startTimes = fulfillments <&> (.start.time.timestamp)
  let endTimes = fulfillments <&> (.end.time.timestamp)
  let schedule =
        filter (isInTheFuture now) $
          zipWith ScheduleElement (NE.toList startTimes) (NE.toList endTimes)
  let firstFulfillment = NE.head fulfillments
  departureStation <- locToStation firstFulfillment.start.location
  arrivalStation <- locToStation firstFulfillment.end.location
  return MetroRide {..}
  where
    locToStation loc = do
      name <- loc.descriptor.name & fromMaybeM (InvalidRequest "descriptor.name is missing")
      return
        MetroStation
          { name = name,
            stationCode = Nothing,
            point = gpsToLatLon loc.gps
          }
    gpsToLatLon Gps {..} = LatLong {..}
    isInTheFuture now scheduleElement = scheduleElement.departureTime > now
