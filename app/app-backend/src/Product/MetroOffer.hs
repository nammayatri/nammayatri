module Product.MetroOffer where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Types.Core.Migration.Gps
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Domain.Types.SearchRequest (SearchRequest)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))
import qualified Tools.Metrics as Metrics
import Types.API.MetroOffer
import Types.CoreMetro.Catalog
import Types.CoreMetro.Item
import Types.CoreMetro.Location
import Types.CoreMetro.Provider

type OnSearch =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

searchCbMetro ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearchCatalog ->
  FlowHandler AckResponse
searchCbMetro _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  Metrics.finishSearchMetrics req.context.transaction_id
  case req.contents of
    Right msg -> setMetroOffers req.context msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

buildContextMetro ::
  (MonadTime m, MonadGuid m, MonadThrow m) =>
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  m Context.Context
buildContextMetro action txnId bapId bapUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { Context.domain = Context.METRO,
        Context.country = "IND",
        Context.city = "Kochi",
        Context.core_version = "0.9.1",
        Context.bap_id = bapId,
        Context.bap_uri = bapUri,
        Context.bpp_id = Nothing,
        Context.bpp_uri = Nothing,
        Context.transaction_id = txnId,
        ..
      }

setMetroOffers ::
  (MonadThrow m, Log m, MonadTime m) =>
  MonadFlow m =>
  Context.Context ->
  Catalog ->
  m ()
setMetroOffers context catalog = do
  let searchReqId = Id context.transaction_id
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
providerToMetroOffer rideSearchId Provider {descriptor, items, locations} = do
  description <- descriptor.name & fromMaybeM (InvalidRequest "Provider is missing descriptor.name")
  rides <- traverse (itemToMetroRide locations) items
  createdAt <- getCurrentTime
  return $ MetroOffer {..}

itemToMetroRide :: (MonadThrow m, MonadTime m, Log m) => [Location] -> Item -> m MetroRide
itemToMetroRide locations item = do
  price <-
    (item.price.value >>= convertDecimalValueToAmount)
      & fromMaybeM (InvalidRequest "Missing price.value in item")
  unless (length item.stops >= 2) $ throwError (InvalidRequest "There must be at least two stops in item")
  let departureStop = head item.stops
  let arrivalStop = last item.stops
  now <- getCurrentTime
  let schedule =
        filter (isInTheFuture now) $
          zipWith ScheduleElement departureStop.time.schedule.times arrivalStop.time.schedule.times
  departureStation <- findLoc departureStop.id >>= locToStation
  arrivalStation <- findLoc arrivalStop.id >>= locToStation
  return MetroRide {..}
  where
    findLoc id =
      find (\loc -> loc.id == id) locations
        & fromMaybeM (InvalidRequest $ "Location " <> id <> " not found in provider.locations")
    locToStation Location {..} = do
      name <- descriptor.name & fromMaybeM (InvalidRequest "descriptor.name is missing")
      return
        MetroStation
          { name = name,
            stationCode = station_code,
            point = gpsToLatLon gps
          }
    gpsToLatLon Gps {..} = LatLong {..}
    isInTheFuture now scheduleElement = scheduleElement.departureTime > now
