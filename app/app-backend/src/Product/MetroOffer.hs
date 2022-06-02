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
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  Metrics.finishSearchMetrics transactionId
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
buildContextMetro action txnId bapId bapUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
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
        transaction_id = Just txnId,
        ..
      }

setMetroOffers ::
  (MonadThrow m, Log m, MonadTime m) =>
  MonadFlow m =>
  Context ->
  Catalog ->
  m ()
setMetroOffers context catalog = do
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  let searchReqId = Id transactionId
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
    realToFrac <$> item.price.value
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
