module Product.MetroOffer where

-- import Beckn.Types.Core.Migration.API.Search

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Core.Migration.API.Types
import Beckn.Types.Core.Migration.Context
import qualified Beckn.Types.Core.Migration.Context as Mig
import Beckn.Types.Core.Migration.DecimalValue
import qualified Beckn.Types.Core.Migration.Domain as Mig
import Beckn.Types.Core.Migration.Gps
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common
import Beckn.Utils.Servant.BaseUrl (showBaseUrlText)
import EulerHS.Prelude hiding (id)
import Types.API.MetroOffer
import Types.CoreMetro.Catalog
import Types.CoreMetro.Item
import Types.CoreMetro.Location
import Types.CoreMetro.Provider
import Types.Metrics (HasBAPMetrics)
import Types.Storage.Case (Case)
import qualified Utils.Metrics as Metrics

searchCbMetro ::
  MonadFlow m =>
  HasBAPMetrics m r =>
  BecknCallbackReq OnSearchCatalog ->
  m AckResponse
searchCbMetro req = withTransactionIdLogTag req $ do
  Metrics.finishSearchMetrics req.context.transaction_id
  case req.contents of
    Right msg -> setMetroOffers req.context msg.catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

buildContextMetro ::
  (MonadTime m, MonadGuid m, MonadThrow m) =>
  Mig.Action ->
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Mig.Context
buildContextMetro action txnId bapUri bppUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
  return
    Mig.Context
      { Mig.domain = Mig.MOBILITY,
        Mig.country = "IND",
        Mig.city = "",
        Mig.core_version = "0.9.1",
        Mig.bap_id = showBaseUrlText bapUri,
        Mig.bap_uri = bapUri,
        Mig.bpp_id = show <$> bppUri,
        Mig.bpp_uri = bppUri,
        Mig.transaction_id = txnId,
        Mig.key = Nothing,
        Mig.ttl = Nothing,
        ..
      }

setMetroOffers ::
  (MonadThrow m, Log m, MonadTime m) =>
  MonadFlow m =>
  Context ->
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
  Id Case ->
  m [a]
getMetroOffers searchReqId =
  fromMaybe [] <$> Redis.getKeyRedis (metroOfferKey searchReqId)

metroOfferKey :: Id Case -> Text
metroOfferKey (Id id') = "BAP:Metro:" <> id'

catalogToMetroOffers :: (MonadThrow m, Log m, MonadTime m) => Id Case -> Catalog -> m [MetroOffer]
catalogToMetroOffers caseId Catalog {bpp_providers} =
  traverse (providerToMetroOffer caseId) bpp_providers

providerToMetroOffer :: (MonadThrow m, Log m, MonadTime m) => Id Case -> Provider -> m MetroOffer
providerToMetroOffer rideSearchId Provider {descriptor, items, locations} = do
  description <- descriptor.name & fromMaybeM (InvalidRequest "Provider is missing descriptor.name")
  rides <- traverse (itemToMetroRide locations) items
  createdAt <- getCurrentTime
  return $ MetroOffer {..}

itemToMetroRide :: (MonadThrow m, Log m) => [Location] -> Item -> m MetroRide
itemToMetroRide locations item = do
  price <-
    (item.price.value >>= convertDecimalValueToAmount)
      & fromMaybeM (InvalidRequest "Missing price.value in item")
  unless (length item.stops >= 2) $ throwError (InvalidRequest "There must be at least two stops in item")
  let stop1 = head item.stops
  let stop2 = last item.stops
  let schedule = zipWith ScheduleElement stop1.time.schedule.times stop2.time.schedule.times
  arrivalStation <- findLoc stop1.id >>= locToStation
  departureStation <- findLoc stop2.id >>= locToStation
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
