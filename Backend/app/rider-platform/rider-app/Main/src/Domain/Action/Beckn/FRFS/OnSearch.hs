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
import BecknV2.FRFS.Utils
import Data.Aeson
-- import Data.Text hiding(map,zip,find,length,null,any)
import Data.List (sortBy)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSSearch as Search
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import qualified Domain.Types.StationType as Station
import EulerHS.Prelude (comparing, toStrict, (+||), (||+))
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CreateFareForMultiModal as SLCF
import qualified SharedLogic.FRFSUtils as SFU
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.PersonStats as QPStats
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

data DVehicleServiceTier = DVehicleServiceTier
  { serviceTierType :: Spec.ServiceTierType,
    serviceTierProviderCode :: Text,
    serviceTierShortName :: Text,
    serviceTierDescription :: Text,
    serviceTierLongName :: Text
  }

data DQuote = DQuote
  { bppItemId :: Text,
    price :: Price,
    vehicleType :: Spec.VehicleCategory,
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
    routeTravelTime :: Maybe Seconds,
    routeSequenceNum :: Maybe Int,
    routeServiceTier :: Maybe DVehicleServiceTier,
    routePrice :: Price,
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

validateRequest :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DOnSearch -> m ValidatedDOnSearch
validateRequest DOnSearch {..} = do
  search <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  let merchantId = search.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow search.merchantOperatingCityId [] >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show search.merchantOperatingCityId)
  if frfsConfig.isEventOngoing == Just True && search.riderId /= SFU.partnerOrgRiderId
    then do
      stats <- QPStats.findByPersonId search.riderId >>= fromMaybeM (InternalError "Person stats not found")
      return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent, isEventOngoing = True, mbFreeTicketInterval = frfsConfig.freeTicketInterval, mbMaxFreeTicketCashback = frfsConfig.maxFreeTicketCashback}
    else return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = 0, isEventOngoing = False, mbFreeTicketInterval = Nothing, mbMaxFreeTicketCashback = Nothing}

onSearch ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  DOnSearch ->
  ValidatedDOnSearch ->
  m ()
onSearch onSearchReq validatedReq = do
  quotesCreatedByCache <- QQuote.findAllBySearchId (Id onSearchReq.transactionId)
  quotes <- traverse (mkQuotes onSearchReq validatedReq) (onSearchReq.quotes)
  traverse_ cacheQuote quotes
  if null quotesCreatedByCache
    then QQuote.createMany quotes
    else do
      zippedQuotes <- verifyAndZipQuotes quotesCreatedByCache quotes
      let updatedQuotes = map updateQuotes zippedQuotes
      for_ updatedQuotes \quote -> QQuote.updateCachedQuoteByPrimaryKey quote
  let search = validatedReq.search
      mbRequiredQuote = filterQuotes quotes
  whenJust mbRequiredQuote $ \requiredQuote -> do
    void $ SLCF.createFares search.id.getId search.journeyLegInfo (QSearch.updatePricingId validatedReq.search.id (Just requiredQuote.id.getId))
  QSearch.updateIsOnSearchReceivedById (Just True) validatedReq.search.id
  return ()
  where
    cacheQuote quote = do
      let key =
            CachedQuote.FRFSCachedQuoteKey
              { CachedQuote.fromStationId = quote.fromStationId,
                CachedQuote.toStationId = quote.toStationId,
                CachedQuote.providerId = quote.providerId,
                CachedQuote.quoteType = quote._type
              }
      CachedQuote.cacheByFRFSCachedQuoteKey key CachedQuote.FRFSCachedQuote {CachedQuote.price = quote.price, CachedQuote.stationsJson = quote.stationsJson}

filterQuotes :: [Quote.FRFSQuote] -> Maybe Quote.FRFSQuote
filterQuotes quotes = listToMaybe quotes

mkQuotes :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DOnSearch -> ValidatedDOnSearch -> DQuote -> m Quote.FRFSQuote
mkQuotes dOnSearch ValidatedDOnSearch {..} DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")
  let merchantOperatingCityId = search.merchantOperatingCityId
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  startStation <- QStation.findByStationCode dStartStation.stationCode integratedBPPConfig.id >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dStartStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  endStation <- QStation.findByStationCode dEndStation.stationCode integratedBPPConfig.id >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dEndStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  let stationsJSON = stations & map castStationToAPI & encodeToText
  let routeStationsJSON = routeStations & map castRouteStationToAPI & encodeToText
  let discountsJSON = discounts & map castDiscountToAPI & encodeToText
  uid <- generateGUID
  now <- getCurrentTime
  let (discountedTickets, eventDiscountAmount) = SFU.getDiscountInfo isEventOngoing mbFreeTicketInterval mbMaxFreeTicketCashback price search.quantity ticketsBookedInEvent
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
        Quote.validTill,
        Quote.vehicleType,
        Quote.merchantId = search.merchantId,
        Quote.merchantOperatingCityId = search.merchantOperatingCityId,
        Quote.partnerOrgId = search.partnerOrgId,
        Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
        Quote.createdAt = now,
        Quote.updatedAt = now,
        bppDelayedInterest = readMaybe . T.unpack =<< dOnSearch.bppDelayedInterest,
        oldCacheDump = Nothing,
        ..
      }

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
      API.towards = Nothing
    }

castRouteStationToAPI :: DRouteStation -> API.FRFSRouteStationsAPI
castRouteStationToAPI DRouteStation {..} =
  API.FRFSRouteStationsAPI
    { API.code = routeCode,
      API.color = routeColor,
      API.startPoint = routeStartPoint,
      API.endPoint = routeEndPoint,
      API.longName = routeLongName,
      API.shortName = routeShortName,
      API.sequenceNum = routeSequenceNum,
      API.travelTime = routeTravelTime,
      API.vehicleServiceTier = castVehicleServiceTierAPI <$> routeServiceTier,
      API.priceWithCurrency = mkPriceAPIEntity routePrice,
      API.stations = map castStationToAPI routeStations
    }

castVehicleServiceTierAPI :: DVehicleServiceTier -> API.FRFSVehicleServiceTierAPI
castVehicleServiceTierAPI DVehicleServiceTier {..} =
  API.FRFSVehicleServiceTierAPI
    { _type = serviceTierType,
      providerCode = serviceTierProviderCode,
      description = serviceTierDescription,
      longName = serviceTierLongName,
      shortName = serviceTierShortName
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

isQuoteChanged :: (Quote.FRFSQuote, Quote.FRFSQuote) -> Bool
isQuoteChanged (quotesFromCache, quotesFromOnSearch) = not $ quotesFromCache.price == quotesFromOnSearch.price && quotesFromCache.stationsJson == quotesFromOnSearch.stationsJson

updateQuotes :: (Quote.FRFSQuote, Quote.FRFSQuote) -> Quote.FRFSQuote
updateQuotes (quotesFromCache, quotesFromOnSearch) = do
  let oldCacheDump =
        if isQuoteChanged (quotesFromCache, quotesFromOnSearch)
          then Just (toJsonText FRFSCachedQuote {price = quotesFromCache.price, stationsJson = quotesFromCache.stationsJson})
          else Nothing

  Quote.FRFSQuote
    { Quote._type = quotesFromCache._type,
      Quote.bppItemId = quotesFromOnSearch.bppItemId,
      Quote.bppSubscriberId = quotesFromOnSearch.bppSubscriberId,
      Quote.bppSubscriberUrl = quotesFromOnSearch.bppSubscriberUrl,
      Quote.fromStationId = quotesFromCache.fromStationId,
      Quote.id = quotesFromCache.id,
      Quote.price = quotesFromOnSearch.price,
      Quote.providerDescription = quotesFromOnSearch.providerDescription,
      Quote.providerId = quotesFromCache.providerId,
      Quote.providerName = quotesFromCache.providerName,
      Quote.quantity = quotesFromOnSearch.quantity,
      Quote.riderId = quotesFromCache.riderId,
      Quote.searchId = quotesFromCache.searchId,
      Quote.stationsJson = quotesFromCache.stationsJson,
      Quote.routeStationsJson = quotesFromOnSearch.routeStationsJson,
      Quote.discountsJson = quotesFromOnSearch.discountsJson,
      Quote.toStationId = quotesFromCache.toStationId,
      Quote.validTill = quotesFromOnSearch.validTill,
      Quote.vehicleType = quotesFromCache.vehicleType,
      Quote.merchantId = quotesFromCache.merchantId,
      Quote.merchantOperatingCityId = quotesFromCache.merchantOperatingCityId,
      Quote.partnerOrgId = quotesFromCache.partnerOrgId,
      Quote.partnerOrgTransactionId = quotesFromCache.partnerOrgTransactionId,
      Quote.createdAt = quotesFromCache.createdAt,
      Quote.updatedAt = quotesFromCache.updatedAt,
      Quote.bppDelayedInterest = quotesFromOnSearch.bppDelayedInterest,
      Quote.oldCacheDump,
      Quote.eventDiscountAmount = quotesFromOnSearch.eventDiscountAmount,
      Quote.discountedTickets = quotesFromOnSearch.discountedTickets
    }
  where
    toJsonText :: FRFSCachedQuote -> Text
    toJsonText cachedQuote = toStrict $ decodeUtf8 $ encode cachedQuote

verifyAndZipQuotes :: (MonadFlow m) => [Quote.FRFSQuote] -> [Quote.FRFSQuote] -> m [(Quote.FRFSQuote, Quote.FRFSQuote)]
verifyAndZipQuotes quotesFromCache quotesFromOnSearch = do
  if length quotesFromCache /= length quotesFromOnSearch
    then throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
    else case (quotesFromCache, quotesFromOnSearch) of
      ([q1], [q2]) ->
        if q1._type == q2._type
          then return [(q1, q2)]
          else throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
      _ -> do
        let isBothQuotesValid = verifyQuote quotesFromCache && verifyQuote quotesFromOnSearch
        if isBothQuotesValid
          then do
            let sortedQ1 = sortBy (comparing (Down . Quote._type)) quotesFromCache
            let sortedQ2 = sortBy (comparing (Down . Quote._type)) quotesFromOnSearch
            return (zip sortedQ1 sortedQ2)
          else throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
  where
    verifyQuote quotes =
      length quotes == 2
        && any (\q -> q._type == Quote.SingleJourney) quotes
        && any (\q -> q._type == Quote.ReturnJourney) quotes
