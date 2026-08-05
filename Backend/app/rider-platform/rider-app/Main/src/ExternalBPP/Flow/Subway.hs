module ExternalBPP.Flow.Subway where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Text as T
import Domain.Types.Beckn.FRFS.OnSearch
import Domain.Types.BecknConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType as Station
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow.Fare as Fare
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest

-- | Same shape as 'ExternalBPP.Flow.Common.search', so it can be dispatched to in its place for
-- CRIS subway searches. CRIS discovers the route alternatives itself from a single "get all fares"
-- call, so the caller supplied route details, network overrides and provider route id are unused.
crisViaRoutesSearch :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
crisViaRoutesSearch merchant merchantOperatingCity integratedBPPConfig bapConfig _mbNetworkHostUrl _mbNetworkId searchReq _routeDetails blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode mbProviderRouteId = do
  quotes <- buildCrisViaRouteQuotes merchant merchantOperatingCity integratedBPPConfig searchReq blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode mbProviderRouteId
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = CallAPI.getProviderName integratedBPPConfig,
        quotes = quotes,
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }

-- | One CRIS "get all fares" call returns every route alternative between the two stations along
-- with its fares. As on the multimodal path, a route is quoted once, on its best fare by the
-- configured sorting criteria.
buildCrisViaRouteQuotes :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> DFRFSSearch.FRFSSearch -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m [DQuote]
buildCrisViaRouteQuotes merchant merchantOperatingCity integratedBPPConfig searchReq blacklistedServiceTiers blacklistedFareQuoteTypes _isSingleMode _mbProviderRouteId = do
  let fareRoute =
        CallAPI.FareRoute
          { segments =
              pure
                CallAPI.BasicRouteDetail
                  { routeCode = "-",
                    startStopCode = searchReq.fromStationCode,
                    endStopCode = searchReq.toStationCode,
                    color = Nothing
                  },
            mbProviderRouteId = Nothing
          }
  (_, fares) <-
    Fare.getFares
      searchReq.riderId
      merchant.id
      merchantOperatingCity.id
      integratedBPPConfig
      fareRoute
      searchReq.vehicleType
      Nothing
      searchReq.multimodalSearchRequestId
      blacklistedServiceTiers
      blacklistedFareQuoteTypes
      True
      True
  let bestFarePerRoute = M.toList $ M.fromListWith keepBetterFare [(fd.providerRouteId, fare) | fare <- fares, Just fd <- [fare.fareDetails]]
  logDebug $ "CRIS via routes for search " <> searchReq.id.getId <> ": " <> show (map fst bestFarePerRoute)
  concat <$> mapM buildRouteQuote (sortRoutes bestFarePerRoute)
  where
    keepBetterFare new old = if compareFares new old == LT then new else old

    compareFares a b =
      case sortingCriteria of
        Just FARE -> compare (adultFare a) (adultFare b) <> compare (routeDistance a) (routeDistance b)
        Just DISTANCE -> compare (routeDistance a) (routeDistance b) <> compare (adultFare a) (adultFare b)
        Nothing -> EQ

    sortRoutes routes =
      case sortingCriteria of
        Just FARE -> sortOn (\(_, fare) -> (adultFare fare, routeDistance fare)) routes
        Just DISTANCE -> sortOn (\(_, fare) -> (routeDistance fare, adultFare fare)) routes
        Nothing -> routes

    sortingCriteria =
      case integratedBPPConfig.providerConfig of
        CRIS crisConfig -> crisConfig.routeSortingCriteria
        _ -> Nothing

    adultFare fare = fromMaybe (HighPrecMoney 0.0) (find (\category -> category.category == ADULT) fare.categories <&> (.price.amount))

    routeDistance fare = fare.fareDetails <&> (.distance)

    buildRouteQuote (providerRouteId, fare) = do
      let viaStops =
            case fare.fareDetails of
              Just fareDetails | not (T.null (T.strip fareDetails.via)) -> T.splitOn "-" (T.strip fareDetails.via)
              _ -> []
      stops <- expandViaPoints $ [searchReq.fromStationCode] <> viaStops <> [searchReq.toStationCode]
      resolvedStations <- mapM (`OTPRest.getStationByGtfsIdAndStopCode` integratedBPPConfig) stops
      case sequence resolvedStations of
        Nothing -> do
          logError $ "Dropping CRIS via route " <> providerRouteId <> ", stations not found for stops: " <> show stops
          return []
        Just stations -> do
          let dStations = zipWith (mkDStation $ length stations) [0 ..] stations
          return [mkRouteQuote integratedBPPConfig searchReq.vehicleType providerRouteId dStations fare]

    -- Seam for corridor station expansion, which today lives on the multimodal path in
    -- getSubwayValidRoutes. Replace with the corridor lookup when it moves over.
    expandViaPoints = pure

    mkDStation totalStops stopSequence station =
      DStation
        { stationCode = station.code,
          stationName = station.name,
          stationLat = station.lat,
          stationLon = station.lon,
          stationType = mkStationType totalStops stopSequence,
          stopSequence = Just stopSequence,
          towards = Nothing,
          color = Nothing
        }

    mkStationType totalStops stopSequence
      | stopSequence == 0 = Station.START
      | stopSequence == totalStops - 1 = Station.END
      | otherwise = Station.TRANSIT

-- | A CRIS via route is a single route through named via stations, not a set of stitched together
-- routes, so the quote carries exactly one route station holding the whole stop list.
mkRouteQuote :: IntegratedBPPConfig -> Spec.VehicleCategory -> Text -> [DStation] -> FRFSFare -> DQuote
mkRouteQuote integratedBPPConfig vehicleType providerRouteId stations FRFSFare {..} =
  let mbAdultCategory = find (\category -> category.category == ADULT) categories
      adultPrice = maybe (Price (Money 0) (HighPrecMoney 0.0) INR) (.price) mbAdultCategory
      adultBppItemId = maybe (CallAPI.getProviderName integratedBPPConfig) (.bppItemId) mbAdultCategory
      mbStartStation = listToMaybe stations
      mbEndStation = listToMaybe (reverse stations)
      latLongOf station = LatLong <$> station.stationLat <*> station.stationLon
      routeStation =
        DRouteStation
          { routeCode = providerRouteId,
            routeLongName = maybe "" (.stationName) mbStartStation <> " - " <> maybe "" (.stationName) mbEndStation,
            routeShortName = providerRouteId,
            routeStartPoint = fromMaybe (LatLong 0 0) (mbStartStation >>= latLongOf),
            routeEndPoint = fromMaybe (LatLong 0 0) (mbEndStation >>= latLongOf),
            routeStations = stations,
            -- Per segment train timings from the nandi timetable are not carried on the quote yet;
            -- populate once the product decision on where they live lands.
            routeTravelTime = Nothing,
            routeSequenceNum = Just 1,
            routeServiceTier = Just $ mkDVehicleServiceTier vehicleServiceTier,
            routePrice = adultPrice,
            routeColor = Nothing
          }
   in DQuote
        { bppItemId = adultBppItemId,
          routeCode = providerRouteId,
          vehicleType = vehicleType,
          _type = DFRFSQuote.SingleJourney,
          routeStations = [routeStation],
          stations = stations,
          fareDetails = fareDetails,
          categories = map mkDCategory categories
        }

mkDVehicleServiceTier :: FRFSVehicleServiceTier -> DVehicleServiceTier
mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

mkDCategory :: FRFSTicketCategory -> DCategory
mkDCategory FRFSTicketCategory {..} = DCategory {..}
