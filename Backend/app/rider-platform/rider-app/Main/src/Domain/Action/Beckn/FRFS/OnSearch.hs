{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnSearch where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSSearch as Search
import Domain.Types.Merchant
import Domain.Types.Route (Route)
import qualified Domain.Types.StationType as Station
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.RouteStopMapping as CQRouteStopMapping
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error

data DOnSearch = DOnSearch
  { bppSubscriberId :: Text,
    bppSubscriberUrl :: Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    providerName :: Text,
    quotes :: [DQuote],
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    bppDelayedInterest :: Maybe Text
  }

data DQuote = DQuote
  { bppItemId :: Text,
    price :: Price,
    vehicleType :: Spec.VehicleCategory,
    serviceTierType :: Maybe Spec.ServiceTierType,
    serviceTierProviderCode :: Maybe Text,
    serviceTierShortName :: Maybe Text,
    serviceTierDescription :: Maybe Text,
    serviceTierLongName :: Maybe Text,
    routeStations :: [DRouteStation],
    stations :: [DStation],
    discounts :: [DDiscount],
    _type :: Quote.FRFSQuoteType
  }

data DDiscount = DDiscount
  { code :: Text,
    title :: Text,
    description :: Text,
    tnc :: Text,
    price :: Price,
    eligibility :: Bool
  }

data DRouteStation = DRouteStation
  { routeCode :: Text,
    routeLongName :: Text,
    routeShortName :: Text,
    routeStartPoint :: LatLong,
    routeEndPoint :: LatLong,
    routeStations :: [DStation],
    routeSequenceNum :: Maybe Int,
    routeColor :: Maybe Text
  }

data DStation = DStation
  { stationCode :: Text,
    stationName :: Text,
    stationLat :: Maybe Double,
    stationLon :: Maybe Double,
    stationType :: Station.StationType,
    stopSequence :: Maybe Int,
    towards :: Maybe Text
  }

data ValidatedDOnSearch = ValidatedDOnSearch
  { merchant :: Merchant,
    search :: Search.FRFSSearch,
    ticketsBookedInEvent :: Int,
    isEventOngoing :: Bool,
    mbFreeTicketInterval :: Maybe Int,
    mbMaxFreeTicketCashback :: Maybe Int
  }

validateRequest :: DOnSearch -> Flow ValidatedDOnSearch
validateRequest DOnSearch {..} = do
  search <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  let merchantId = search.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId search.merchantOperatingCityId >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show search.merchantOperatingCityId)
  if frfsConfig.isEventOngoing == Just True
    then do
      stats <- QPStats.findByPersonId search.riderId >>= fromMaybeM (InternalError "Person stats not found")
      return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent, isEventOngoing = True, mbFreeTicketInterval = frfsConfig.freeTicketInterval, mbMaxFreeTicketCashback = frfsConfig.maxFreeTicketCashback}
    else return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = 0, isEventOngoing = False, mbFreeTicketInterval = Nothing, mbMaxFreeTicketCashback = Nothing}

onSearch ::
  DOnSearch ->
  ValidatedDOnSearch ->
  Flow ()
onSearch onSearchReq validatedReq = do
  quotes <- traverse (mkQuotes onSearchReq validatedReq) (onSearchReq.quotes)
  QQuote.createMany quotes
  return ()

mkQuotes :: DOnSearch -> ValidatedDOnSearch -> DQuote -> Flow Quote.FRFSQuote
mkQuotes dOnSearch ValidatedDOnSearch {..} DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")

  startStation <- QStation.findByStationCode dStartStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dStartStation.stationCode)
  endStation <- QStation.findByStationCode dEndStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dEndStation.stationCode)
  (routeStations', routeId, routes) <-
    case vehicleType of
      Spec.BUS -> do
        let maybeFirstRouteStation = listToMaybe routeStations
        case maybeFirstRouteStation of
          Nothing -> pure (routeStations, Nothing, Nothing)
          Just rStation -> do
            route <- QRoute.findByRouteCode rStation.routeCode >>= fromMaybeM (RouteNotFound rStation.routeCode)
            return (routeStations, Just (Quote.Bus route.id), Nothing)
      Spec.METRO -> do
        -- routeStations will be empty in case of metro to keep ACL pure
        metroRouteStations <- mkRouteStations stations
        routes <-
          mapM
            ( \rStation -> QRoute.findByRouteCode rStation.routeCode >>= fromMaybeM (RouteNotFound rStation.routeCode)
            )
            $ sortOn (.routeSequenceNum) metroRouteStations
        return (metroRouteStations, Just (Quote.Metro (map (.id) routes)), Just routes)
  let freeTicketInterval = fromMaybe (maxBound :: Int) mbFreeTicketInterval
  let stationsJSON = stations & map castStationToAPI & encodeToText
  routeStationsJSON <- encodeToText <$> mapM (castRouteStationToAPI routes) routeStations'
  let discountsJSON = discounts & map castDiscountToAPI & encodeToText
  let maxFreeTicketCashback = fromMaybe 0 mbMaxFreeTicketCashback
  uid <- generateGUID
  now <- getCurrentTime
  (discountedTickets, eventDiscountAmount) <-
    if isEventOngoing
      then do
        let perTicketCashback = min maxFreeTicketCashback price.amountInt.getMoney
            discountedTickets = ((ticketsBookedInEvent + search.quantity) `div` freeTicketInterval) - (ticketsBookedInEvent `div` freeTicketInterval)
            eventDiscountAmount = toHighPrecMoney $ discountedTickets * perTicketCashback
        return (Just discountedTickets, Just eventDiscountAmount)
      else return (Nothing, Nothing)
  let validTill = fromMaybe (addUTCTime (intToNominalDiffTime 900) now) dOnSearch.validTill -- If validTill is not present, set it to 15 minutes from now
  return
    Quote.FRFSQuote
      { Quote._type = _type,
        Quote.bppItemId,
        Quote.bppSubscriberId = dOnSearch.bppSubscriberId,
        Quote.bppSubscriberUrl = dOnSearch.bppSubscriberUrl,
        Quote.fromStationId = startStation.id,
        Quote.id = uid,
        Quote.price,
        Quote.providerDescription = dOnSearch.providerDescription,
        Quote.providerId = dOnSearch.providerId,
        Quote.providerName = dOnSearch.providerName,
        Quote.quantity = search.quantity,
        Quote.riderId = search.riderId,
        Quote.searchId = search.id,
        Quote.stationsJson = stationsJSON,
        Quote.routeStationsJson = Just routeStationsJSON,
        Quote.discountsJson = Just discountsJSON,
        Quote.toStationId = endStation.id,
        Quote.routeId = routeId,
        Quote.validTill,
        Quote.vehicleType,
        Quote.merchantId = search.merchantId,
        Quote.merchantOperatingCityId = search.merchantOperatingCityId,
        Quote.partnerOrgId = search.partnerOrgId,
        Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
        Quote.createdAt = now,
        Quote.updatedAt = now,
        bppDelayedInterest = readMaybe . T.unpack =<< dOnSearch.bppDelayedInterest,
        ..
      }

mkRouteStations :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DStation] -> m [DRouteStation]
mkRouteStations allStations = do
  concat
    <$> mapM
      ( \station -> do
          routeStationMappings <- QRouteStopMapping.findByStopCode station.stationCode
          mapM
            ( \routeStationMapping -> do
                route <- QRoute.findByRouteCode routeStationMapping.routeCode >>= fromMaybeM (RouteNotFound routeStationMapping.routeCode)
                allRouteStations <- QRouteStopMapping.findByRouteCode route.code
                let routeStations = filter (\station' -> station'.stationCode `elem` map (.stopCode) allRouteStations) allStations
                return $
                  DRouteStation
                    { routeCode = route.code,
                      routeLongName = route.longName,
                      routeShortName = route.shortName,
                      routeStartPoint = route.startPoint,
                      routeEndPoint = route.endPoint,
                      routeStations = routeStations,
                      routeSequenceNum = Just routeStationMapping.sequenceNum,
                      routeColor = route.color
                    }
            )
            routeStationMappings
      )
      allStations

getStartStation :: [DStation] -> Maybe DStation
getStartStation = find (\station -> station.stationType == Station.START)

getEndStation :: [DStation] -> Maybe DStation
getEndStation = find (\station -> station.stationType == Station.END)

castStationToAPI :: DStation -> API.FRFSStationAPI
castStationToAPI DStation {..} =
  API.FRFSStationAPI
    { API.address = Nothing,
      API.code = stationCode,
      API.color = Nothing,
      API.lat = stationLat,
      API.lon = stationLon,
      API.name = stationName,
      API.stationType = Just stationType,
      API.sequenceNum = stopSequence,
      API.distance = Nothing,
      API.towards = Nothing,
      API.routeName = Nothing
    }

castStationToAPI' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe [Route] -> DRouteStation -> DStation -> m API.FRFSStationAPI
castStationToAPI' allRoutes route station@DStation {..} = do
  towards' <- if stationType == Station.TRANSIT then evaluateTowardsStation station route allRoutes else pure Nothing
  pure $
    API.FRFSStationAPI
      { API.address = Nothing,
        API.code = stationCode,
        API.color = Nothing,
        API.lat = stationLat,
        API.lon = stationLon,
        API.name = stationName,
        API.stationType = Just stationType,
        API.sequenceNum = stopSequence,
        API.distance = Nothing,
        API.towards = towards',
        API.routeName = Just route.routeLongName
      }

castRouteStationToAPI :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe [Route] -> DRouteStation -> m API.FRFSRouteStationsAPI
castRouteStationToAPI allRoutes route@DRouteStation {..} = do
  stations <- mapM (castStationToAPI' allRoutes route) routeStations
  pure $
    API.FRFSRouteStationsAPI
      { API.code = routeCode,
        API.color = routeColor,
        API.startPoint = routeStartPoint,
        API.endPoint = routeEndPoint,
        API.longName = routeLongName,
        API.shortName = routeShortName,
        API.sequenceNum = routeSequenceNum,
        API.stations = stations
      }

castDiscountToAPI :: DDiscount -> API.FRFSDiscountRes
castDiscountToAPI DDiscount {..} =
  API.FRFSDiscountRes
    { API.code = code,
      API.price = mkPriceAPIEntity price,
      API.title = title,
      API.description = description,
      API.tnc = tnc,
      API.eligibility = eligibility
    }

evaluateTowardsStation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DStation -> DRouteStation -> Maybe [Route] -> m (Maybe Text)
evaluateTowardsStation DStation {..} route allRoutes = do
  let filteredRoute = allRoutes >>= find (\r -> r.code == route.routeCode)
  case (filteredRoute, towards) of
    (Just requiredRoute, Just nextStationCode) -> do
      nextStop <- CQRouteStopMapping.findByRouteCodeAndStopCode route.routeCode nextStationCode >>= fromMaybeM (InternalError $ "Route Station Mapping not found: " <> nextStationCode)
      currentStop <- CQRouteStopMapping.findByRouteCodeAndStopCode route.routeCode stationCode >>= fromMaybeM (InternalError $ "Route Station Mapping not found: " <> stationCode)
      return $ if currentStop.sequenceNum < nextStop.sequenceNum then requiredRoute.lastStopName else requiredRoute.firstStopName
    _ -> pure Nothing
