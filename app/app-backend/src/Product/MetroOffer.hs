module Product.MetroOffer where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Metro.API.OnSearch (OnSearchReq)
import Beckn.Types.Core.Metro.OnSearch
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Data.Traversable
import Domain.Types.SearchRequest (SearchRequest)
import EulerHS.Prelude hiding (id)
import qualified Tools.Metrics as Metrics
import Types.API.MetroOffer

searchCbMetro ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearchReq ->
  FlowHandler AckResponse
searchCbMetro _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  Metrics.finishSearchMetrics req.context.message_id
  case req.contents of
    Right msg -> setMetroOffers req.context msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

buildContextMetro ::
  (MonadTime m, MonadGuid m, MonadThrow m) =>
  Action ->
  Text ->
  Text ->
  BaseUrl ->
  m Context
buildContextMetro action message_id bapId bapUri = do
  timestamp <- getCurrentTime
  return
    Context
      { domain = METRO,
        country = "IND",
        city = "Kochi",
        core_version = "0.9.1",
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = Nothing,
        bpp_uri = Nothing,
        transaction_id = Nothing,
        ..
      }

setMetroOffers ::
  (MonadThrow m, Log m, MonadTime m) =>
  MonadFlow m =>
  Context ->
  Catalog ->
  m ()
setMetroOffers context catalog = do
  let searchReqId = Id context.message_id
  val <- catalogToMetroOffers searchReqId catalog
  Redis.setExRedis (metroOfferKey searchReqId) val (60 * 60 * 24)

getMetroOffers ::
  ( MonadFlow m,
    FromJSON a
  ) =>
  Id SearchRequest ->
  m [a]
getMetroOffers searchReqId =
  fromMaybe [] <$> Redis.getKeyRedis (metroOfferKey searchReqId)

metroOfferKey :: Id SearchRequest -> Text
metroOfferKey (Id id') = "BAP:Metro:" <> id'

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

uniteItemAndFulfillment :: (MonadThrow m, Log m) => [Item] -> [Fulfillment] -> m [(Item, [Fulfillment])]
uniteItemAndFulfillment items fulfillments = do
  items `for` \item -> do
    ff <- findFulfillments item.fulfillment_id
    return (item, ff)
  where
    findFulfillments id = do
      let fulfilments = filter (\fulfillment -> fulfillment.id == id) fulfillments
      when (null fulfilments) . throwError . InvalidRequest $ "Fulfillment " <> id <> " not found in provider.fulfillments"
      return fulfilments

offerInfoToMetroRide :: (MonadTime m, MonadThrow m, Log m) => (Item, [Fulfillment]) -> m MetroRide
offerInfoToMetroRide (item, fulfillments) = do
  price <-
    realToFrac <$> item.price.value
      & fromMaybeM (InvalidRequest "Missing price.value in item")
  now <- getCurrentTime
  let schedule =
        filter (isInTheFuture now) $
          zipWith ScheduleElement (fulfillments <&> (.start.time.timestamp)) (fulfillments <&> (.end.time.timestamp))
  fulfillment <- listToMaybe fulfillments & fromMaybeM (InternalError "Empty fulfillments list.") --Already checkeed that list is not empty.
  departureStation <- locToStation fulfillment.start.location
  arrivalStation <- locToStation fulfillment.end.location
  return MetroRide {..}
  where
    locToStation loc = do
      name <- loc.descriptor.name & fromMaybeM (InvalidRequest "descriptor.name is missing")
      return
        MetroStation
          { name = name,
            stationCode = loc.code,
            point = gpsToLatLon loc.gps
          }
    gpsToLatLon Gps {..} = LatLong {..}
    isInTheFuture now scheduleElement = scheduleElement.departureTime > now
