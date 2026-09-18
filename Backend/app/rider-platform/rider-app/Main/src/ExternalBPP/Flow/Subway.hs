module ExternalBPP.Flow.Subway where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub, sortOn)
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
import Domain.Utils (mapConcurrently)
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow.Fare as Fare
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Tools.Metrics.BAPMetrics as Metrics

crisViaRoutesSearch :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, Metrics.HasBAPMetrics m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r, Forkable m) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Maybe BaseUrl -> Maybe Text -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m DOnSearch
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

buildCrisViaRouteQuotes :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, Metrics.HasBAPMetrics m r, HasShortDurationRetryCfg r c, HasMasterCloudForwarder r, Forkable m) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> DFRFSSearch.FRFSSearch -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Maybe Text -> m [DQuote]
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
  let bestFarePerPath = M.toList $ M.fromListWith mergeSamePath [(mkStopPath fd, (fare, fd.providerRouteId, [])) | fare <- fares, Just fd <- [fare.fareDetails]]
  logDebug $ "CRIS via routes for search " <> searchReq.id.getId <> ": " <> show (map (\(stops, (_, routeId, alternates)) -> (stops, routeId, alternates)) bestFarePerPath)
  resolvedPaths <- catMaybes <$> mapConcurrently resolveIfServable bestFarePerPath
  let bestFarePerJourney = M.toList $ M.fromListWith mergeSamePath resolvedPaths
  logDebug $ "CRIS journeys for search " <> searchReq.id.getId <> ": " <> show (length bestFarePerJourney) <> " from " <> show (length resolvedPaths) <> " servable of " <> show (length bestFarePerPath) <> " paths"
  concat <$> mapConcurrently buildRouteQuoteSafely (sortRoutes bestFarePerJourney)
  where
    mkStopPath fareDetails = dropAdjacentDuplicates $ [searchReq.fromStationCode] <> splitVia fareDetails.via <> [searchReq.toStationCode]

    splitVia via = if T.null (T.strip via) then [] else filter (not . T.null) (map T.strip (T.splitOn "-" (T.strip via)))

    dropAdjacentDuplicates = foldr dropIfSameAsNext []
      where
        dropIfSameAsNext stopCode acc =
          case acc of
            nextStopCode : _ | nextStopCode == stopCode -> acc
            _ -> stopCode : acc

    mergeSamePath (newFare, newRouteId, newAlternates) (oldFare, oldRouteId, oldAlternates)
      | compareFares newFare oldFare == LT = (newFare, newRouteId, nub (oldRouteId : oldAlternates <> newAlternates))
      | otherwise = (oldFare, oldRouteId, nub (newRouteId : newAlternates <> oldAlternates))

    compareFares a b =
      case sortingCriteria of
        Just FARE -> compare (adultFare a) (adultFare b) <> compare (routeDistance a) (routeDistance b)
        Just DISTANCE -> compare (routeDistance a) (routeDistance b) <> compare (adultFare a) (adultFare b)
        Nothing -> EQ

    sortRoutes routes =
      case sortingCriteria of
        Just FARE -> sortOn (\(_, (fare, _, _)) -> (adultFare fare, routeDistance fare)) routes
        Just DISTANCE -> sortOn (\(_, (fare, _, _)) -> (routeDistance fare, adultFare fare)) routes
        Nothing -> routes

    sortingCriteria =
      case integratedBPPConfig.providerConfig of
        CRIS crisConfig -> crisConfig.routeSortingCriteria
        _ -> Nothing

    adultFare fare = fromMaybe (HighPrecMoney 0.0) (find (\category -> category.category == ADULT) fare.categories <&> (.price.amount))

    routeDistance fare = fare.fareDetails <&> (.distance)

    resolveIfServable (stopPath, fareEntry@(_, providerRouteId, _)) = do
      eSegments <- withTryCatch "CRIS:resolveSegments" (resolveSegments stopPath)
      case eSegments of
        Right (Just segments) -> return (Just (segments, fareEntry))
        Right Nothing -> return Nothing
        Left err -> do
          logError $ "Dropping CRIS via route " <> providerRouteId <> ", serviceability check failed for " <> show stopPath <> ": " <> show err
          return Nothing

    resolveSegments stopPath = do
      perSegment <- forM (zip stopPath (drop 1 stopPath)) $ \(fromCode, toCode) -> do
        routeCodes <- JMU.getRouteCodesFromTo fromCode toCode integratedBPPConfig
        when (null routeCodes) $
          logInfo $ "CRIS path " <> show stopPath <> " not servable for search " <> searchReq.id.getId <> ": no route between " <> fromCode <> " and " <> toCode
        return (CrisSegment fromCode toCode <$> listToMaybe routeCodes)
      return (collapseSameRoute <$> sequence perSegment)

    collapseSameRoute (leg : nextLeg : rest)
      | leg.routeCode == nextLeg.routeCode = collapseSameRoute (CrisSegment leg.fromStopCode nextLeg.toStopCode leg.routeCode : rest)
      | otherwise = leg : collapseSameRoute (nextLeg : rest)
    collapseSameRoute segments = segments

    buildRouteQuoteSafely entry@(_, (_, providerRouteId, _)) = do
      eQuotes <- withTryCatch "CRIS:buildRouteQuote" (buildRouteQuote entry)
      case eQuotes of
        Right quotes -> return quotes
        Left err -> do
          logError $ "Dropping CRIS via route " <> providerRouteId <> ", quote build failed: " <> show err
          return []

    buildRouteQuote (segments, (fare, providerRouteId, alternateRouteIds)) = do
      unless (null alternateRouteIds) $
        logInfo $ "CRIS route " <> providerRouteId <> " for search " <> searchReq.id.getId <> " shares its journey with " <> show alternateRouteIds <> ", quoting once"
      mbRoutes <- mapM (\segment -> OTPRest.getRouteByRouteId integratedBPPConfig segment.routeCode) segments
      case sequence mbRoutes of
        Nothing -> do
          logError $ "Dropping CRIS via route " <> providerRouteId <> ", route details not found for " <> show (map (.routeCode) segments)
          return []
        Just routes -> do
          let basicRouteDetails =
                zipWith
                  (\segment route -> CallAPI.BasicRouteDetail {routeCode = segment.routeCode, startStopCode = segment.fromStopCode, endStopCode = segment.toStopCode, color = route.color})
                  segments
                  routes
          stationsPerSegment <- CallAPI.buildStationsPerSegment basicRouteDetails integratedBPPConfig
          let adultPrice = maybe (Price (Money 0) (HighPrecMoney 0.0) INR) (.price) (find (\category -> category.category == ADULT) fare.categories)
              routeStations =
                zipWith3
                  ( \routeSeqNum route segmentStations ->
                      DRouteStation
                        { routeCode = route.code,
                          routeLongName = route.longName,
                          routeShortName = route.shortName,
                          routeStartPoint = route.startPoint,
                          routeEndPoint = route.endPoint,
                          routeStations = segmentStations,
                          routeTravelTime = Nothing,
                          routeServiceTier = Just $ mkDVehicleServiceTier fare.vehicleServiceTier,
                          routePrice = adultPrice,
                          routeSequenceNum = Just routeSeqNum,
                          routeColor = route.color
                        }
                  )
                  [1 ..]
                  routes
                  stationsPerSegment
          return [mkRouteQuote integratedBPPConfig searchReq.vehicleType providerRouteId routeStations (concat stationsPerSegment) fare]

data CrisSegment = CrisSegment
  { fromStopCode :: Text,
    toStopCode :: Text,
    routeCode :: Text
  }
  deriving (Eq, Ord, Show)

mkRouteQuote :: IntegratedBPPConfig -> Spec.VehicleCategory -> Text -> [DRouteStation] -> [DStation] -> FRFSFare -> DQuote
mkRouteQuote integratedBPPConfig vehicleType providerRouteId routeStations stations FRFSFare {..} =
  let mbAdultCategory = find (\category -> category.category == ADULT) categories
      adultBppItemId = maybe (CallAPI.getProviderName integratedBPPConfig) (.bppItemId) mbAdultCategory
   in DQuote
        { bppItemId = adultBppItemId,
          routeCode = providerRouteId,
          vehicleType = vehicleType,
          _type = DFRFSQuote.SingleJourney,
          routeStations = routeStations,
          stations = stations,
          fareDetails = fareDetails,
          categories = map mkDCategory categories
        }

mkDVehicleServiceTier :: FRFSVehicleServiceTier -> DVehicleServiceTier
mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

mkDCategory :: FRFSTicketCategory -> DCategory
mkDCategory FRFSTicketCategory {..} = DCategory {..}
