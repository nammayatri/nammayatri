{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnSearch
  ( module Domain.Action.Beckn.FRFS.OnSearch,
    module Reexport,
  )
where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import Data.List (partition, sortBy)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Beckn.FRFS.OnSearch
import qualified Domain.Types.Beckn.FRFS.OnSearch as Reexport
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import qualified Domain.Types.FRFSFarePolicy as FRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSRouteFareProduct as FRFSRouteFareProduct
import qualified Domain.Types.FRFSVehicleServiceTier as FRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType as Station
import qualified Domain.Types.StopFare as StopFare
import qualified Domain.Types.Trip as DTripTypes
import EulerHS.Prelude (comparing, toStrict)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified SharedLogic.CreateFareForMultiModal as SLCF
import qualified SharedLogic.FRFSUtils as SFU
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQVSR
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSFarePolicy as QFFP
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRouteFareProduct as QFRFP
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketCategoryMetadataConfig as QFRFSTicketCategoryMetadataConfig
import qualified Storage.Queries.FRFSVehicleServiceTier as QVSR
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.StopFare as QRSF
import qualified Storage.Queries.Transformers.FRFSQuoteCategory as TFQC
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

validateRequest :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DOnSearch -> m ValidatedDOnSearch
validateRequest DOnSearch {..} = do
  search <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  let merchantId = search.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity search
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow search.merchantOperatingCityId [] >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show search.merchantOperatingCityId)
  if frfsConfig.isEventOngoing == Just True && search.riderId /= SFU.partnerOrgRiderId
    then do
      stats <- QPStats.findByPersonId search.riderId >>= fromMaybeM (InternalError "Person stats not found")
      return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent, isEventOngoing = True, mbFreeTicketInterval = frfsConfig.freeTicketInterval, mbMaxFreeTicketCashback = frfsConfig.maxFreeTicketCashback, integratedBppConfig}
    else return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = 0, isEventOngoing = False, mbFreeTicketInterval = Nothing, mbMaxFreeTicketCashback = Nothing, integratedBppConfig}

onSearch ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    Metrics.HasBAPMetrics m r
  ) =>
  DOnSearch ->
  ValidatedDOnSearch ->
  m ()
onSearch onSearchReq validatedReq = do
  Metrics.finishMetrics Metrics.SEARCH_FRFS validatedReq.merchant.name validatedReq.search.id.getId validatedReq.search.merchantOperatingCityId.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity validatedReq.search
  case integratedBPPConfig.providerConfig of
    DIBC.ONDC _ -> do
      updatedQuotes <- mapM (updateQuote integratedBPPConfig) onSearchReq.quotes
      onSearchHelper (onSearchReq {quotes = updatedQuotes}) validatedReq integratedBPPConfig
    _ -> do
      onSearchHelper onSearchReq validatedReq integratedBPPConfig
  where
    updateQuote integratedBPPConfig quote = do
      stations <-
        forM quote.stations \station -> do
          stationCode <- OTPRest.getStopCodeFromProviderCode integratedBPPConfig station.stationCode
          return $ station {stationCode = fromMaybe station.stationCode stationCode}
      return $ quote {stations = stations}

onSearchHelper :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasShortDurationRetryCfg r c) => DOnSearch -> ValidatedDOnSearch -> DIBC.IntegratedBPPConfig -> m ()
onSearchHelper onSearchReq validatedReq integratedBPPConfig = do
  quotesCreatedByCache <- QQuote.findAllBySearchId (Id onSearchReq.transactionId)
  mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just onSearchReq.transactionId)
  quotesWithCategories <- traverse (mkQuotes onSearchReq validatedReq) onSearchReq.quotes
  let quotes = map fst quotesWithCategories
      quoteCategories = concatMap snd quotesWithCategories
  traverse_ cacheQuote quotesWithCategories
  if null quotesCreatedByCache
    then QQuote.createMany quotes
    else do
      quotesCreatedByCacheWithQuoteCategories <-
        mapM
          ( \quote -> do
              quoteCategories' <- QFRFSQuoteCategory.findAllByQuoteId quote.id
              return (quote, quoteCategories')
          )
          quotesCreatedByCache
      zippedQuotesWithQuoteCategories <- verifyAndZipQuotes quotesCreatedByCacheWithQuoteCategories quotesWithCategories
      let updatedQuotes = map updateQuotes zippedQuotesWithQuoteCategories
      for_ updatedQuotes QQuote.updateCachedQuoteByPrimaryKey
  QFRFSQuoteCategory.createMany quoteCategories
  let search = validatedReq.search
  mbRequiredQuote <- filterQuotes integratedBPPConfig quotesWithCategories mbJourneyLeg
  case mbRequiredQuote of
    Just (requiredQuote, _requiredQuoteCategories) -> do
      void $ SLCF.createFares search.id.getId requiredQuote.id.getId
    Nothing -> do
      QSearch.updateOnSearchFailed validatedReq.search.id (Just True)
  QSearch.updateIsOnSearchReceivedById (Just True) validatedReq.search.id
  fork "Updating Route Stop Fare" $ do
    forM_ onSearchReq.quotes $ \quote -> do
      -- Only cache Single Journey tickets, ignore Return Journey tickets
      when (quote._type == Quote.SingleJourney) $ do
        -- This `null quote.routeStation` check is to ensure that we only update the fare for the route stations if they are present in the quote.
        dStartStation <- getStartStation quote.stations & fromMaybeM (InternalError "Start station not found")
        dEndStation <- getEndStation quote.stations & fromMaybeM (InternalError "End station not found")
        if null quote.routeStations
          then do
            if quote.vehicleType == Spec.METRO
              then do
                QRSF.findAllByStartStopAndIntegratedBPPConfigId dStartStation.stationCode dEndStation.stationCode integratedBPPConfig.id >>= \case
                  fareProducts@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) fareProducts
                    forM_ quote.categories $ \quoteCategory -> do
                      traverse_ (\fp -> upsertStopFare fp dStartStation.stationCode dEndStation.stationCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                  [] -> do
                    QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                      fareProducts@(_ : _) -> do
                        let farePolicyIds = map (.farePolicyId) fareProducts
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> createStopFare farePolicyId dStartStation.stationCode dEndStation.stationCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                      [] -> do
                        -- No fare products exist at all, create everything from scratch
                        createEntriesInFareTables search.merchantId search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id
              else do
                QFRFP.findByRouteCode quote.routeCode integratedBPPConfig.id >>= \case
                  fareProducts@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) fareProducts
                    farePolicies <- QFFP.findAllByIds farePolicyIds
                    let filteredFarePolicies = filter (\fp -> fp._type == FRFSFarePolicy.MatrixBased) farePolicies
                    forM_ quote.categories $ \quoteCategory -> do
                      traverse_
                        ( \fp -> do
                            upsertStopFare fp.id dStartStation.stationCode dEndStation.stationCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id
                        )
                        filteredFarePolicies
                  [] -> do
                    QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                      fareProducts@(_ : _) -> do
                        let farePolicyIds = map (.farePolicyId) fareProducts
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> createStopFare farePolicyId dStartStation.stationCode dEndStation.stationCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                      [] -> do
                        createEntriesInFareTables search.merchantId search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id
          else do
            forM_ quote.routeStations $ \routeStation -> do
              let mbStartStopCode = find (\station -> station.stationType == Station.START) routeStation.routeStations <&> (.stationCode)
                  mbEndStopCode = find (\station -> station.stationType == Station.END) routeStation.routeStations <&> (.stationCode)
              case routeStation.routeFarePolicyId of
                Just farePolicyId -> do
                  -- Update existing fare entry for ALL categories
                  whenJust ((,) <$> mbStartStopCode <*> mbEndStopCode) $ \(startStopCode, endStopCode) -> do
                    forM_ quote.categories $ \quoteCategory -> do
                      upsertStopFare farePolicyId startStopCode endStopCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id
                Nothing -> do
                  -- No farePolicyId means we need to check if fare products exist before creating new ones
                  -- Check if ANY fare products exist for this integratedBPPConfig
                  QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                    fareProducts@(_ : _) -> do
                      let farePolicyIds = map (.farePolicyId) fareProducts
                      whenJust ((,) <$> mbStartStopCode <*> mbEndStopCode) $ \(startStopCode, endStopCode) ->
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> createStopFare farePolicyId startStopCode endStopCode quoteCategory search.merchantId search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                    [] -> do
                      createEntriesInFareTables search.merchantId search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id
  return ()
  where
    cacheQuote (quote, quoteCategories) = do
      whenJust (find (\category -> category.category == ADULT) quoteCategories <&> (.price)) $ \price -> do
        let key =
              CachedQuote.FRFSCachedQuoteKey
                { CachedQuote.fromStationId = quote.fromStationCode,
                  CachedQuote.toStationId = quote.toStationCode,
                  CachedQuote.providerId = quote.providerId,
                  CachedQuote.quoteType = quote._type
                }
        CachedQuote.cacheByFRFSCachedQuoteKey key CachedQuote.FRFSCachedQuote {CachedQuote.price = price, CachedQuote.stationsJson = quote.stationsJson}

-- Upsert fare cache when fareCachingAllowed is enabled
upsertFareCache ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    Metrics.HasBAPMetrics m r
  ) =>
  DOnSearch ->
  ValidatedDOnSearch ->
  m ()
upsertFareCache onSearchReq validatedReq = do
  logInfo $ "Upserting fare cache for search: " <> onSearchReq.transactionId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity validatedReq.search

  updatedQuotes <- case integratedBPPConfig.providerConfig of
    DIBC.ONDC _ -> mapM (updateQuote integratedBPPConfig) onSearchReq.quotes
    _ -> pure onSearchReq.quotes

  let updatedOnSearchReq = onSearchReq {quotes = updatedQuotes}

  -- Find existing quotes created for this search
  quotesCreatedByCache <- QQuote.findAllBySearchId (Id onSearchReq.transactionId)
  quotesWithCategories <- traverse (mkQuotes updatedOnSearchReq validatedReq) updatedOnSearchReq.quotes

  let quoteCategories = concatMap snd quotesWithCategories

  -- Cache quotes in Redis
  traverse_ cacheQuote quotesWithCategories

  -- Upsert quotes: If quotes exist for this searchId, update them; otherwise create new entries
  if null quotesCreatedByCache
    then do
      -- No existing quotes found, create new ones
      logInfo $ "No existing quotes found for search " <> onSearchReq.transactionId <> ", creating new quotes"
      let quotes = map fst quotesWithCategories
      QQuote.createMany quotes
      QFRFSQuoteCategory.createMany quoteCategories
    else do
      -- Existing quotes found: update matched ones and create any new quotes from BPP that we don't have yet
      logInfo $ "Found " <> show (length quotesCreatedByCache) <> " existing quotes for search " <> onSearchReq.transactionId <> ", updating them"
      quotesCreatedByCacheWithQuoteCategories <-
        mapM
          ( \quote -> do
              quoteCategories' <- QFRFSQuoteCategory.findAllByQuoteId quote.id
              return (quote, quoteCategories')
          )
          quotesCreatedByCache
      let (zippedQuotesWithQuoteCategories, newQuotesFromBpp) = matchQuotesForUpsert quotesCreatedByCacheWithQuoteCategories quotesWithCategories

      for_ zippedQuotesWithQuoteCategories updateQuoteCategoriesFromOnSearch
      -- Create any new quotes from ONDC on_search that don't have a cache entry yet
      when (not $ null newQuotesFromBpp) $ do
        logInfo $ "Creating " <> show (length newQuotesFromBpp) <> " new quote(s) from BPP on_search for search " <> onSearchReq.transactionId
        let newQuotes = map fst newQuotesFromBpp
            newQuoteCategories = concatMap snd newQuotesFromBpp
        QQuote.createMany newQuotes
        QFRFSQuoteCategory.createMany newQuoteCategories

  -- Update search status
  QSearch.updateIsOnSearchReceivedById (Just True) validatedReq.search.id

  let search = validatedReq.search
  mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just onSearchReq.transactionId)
  mbRequiredQuote <- filterQuotes integratedBPPConfig quotesWithCategories mbJourneyLeg
  case mbRequiredQuote of
    Just (requiredQuote, _requiredQuoteCategories) -> do
      void $ SLCF.createFares search.id.getId requiredQuote.id.getId
    Nothing -> do
      QSearch.updateOnSearchFailed validatedReq.search.id (Just True)

  -- Refresh fare cache (StopFare table)
  fork "Refreshing Route Stop Fare Cache" $ do
    logDebug $ "[FARE_CACHE_REFRESH] Starting fare cache refresh for " <> show (length updatedOnSearchReq.quotes) <> " quotes"
    forM_ updatedOnSearchReq.quotes $ \quote -> do
      when (quote._type == Quote.SingleJourney) $ do
        dStartStation <- getStartStation quote.stations & fromMaybeM (InternalError "Start station not found")
        dEndStation <- getEndStation quote.stations & fromMaybeM (InternalError "End station not found")

        if null quote.routeStations
          then do
            if quote.vehicleType == Spec.METRO
              then do
                -- METRO: Update fare by start/end stop codes
                QRSF.findAllByStartStopAndIntegratedBPPConfigId dStartStation.stationCode dEndStation.stationCode integratedBPPConfig.id >>= \case
                  fareProducts@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) fareProducts
                    forM_ quote.categories $ \quoteCategory ->
                      traverse_ (\fp -> upsertStopFare fp dStartStation.stationCode dEndStation.stationCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                  [] -> do
                    -- Create new fare entries if none exist
                    QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                      fareProducts@(_ : _) -> do
                        let farePolicyIds = map (.farePolicyId) fareProducts
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> createStopFare farePolicyId dStartStation.stationCode dEndStation.stationCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                      [] ->
                        createEntriesInFareTables validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id
              else do
                -- BUS: Update fare by routeCode
                QFRFP.findByRouteCode quote.routeCode integratedBPPConfig.id >>= \case
                  fareProducts@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) fareProducts
                    farePolicies <- QFFP.findAllByIds farePolicyIds
                    let filteredFarePolicies = filter (\fp -> fp._type == FRFSFarePolicy.MatrixBased) farePolicies
                    forM_ quote.categories $ \quoteCategory ->
                      traverse_ (\fp -> upsertStopFare fp.id dStartStation.stationCode dEndStation.stationCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id) filteredFarePolicies
                  [] -> do
                    QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                      fareProducts@(_ : _) -> do
                        let farePolicyIds = map (.farePolicyId) fareProducts
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> createStopFare farePolicyId dStartStation.stationCode dEndStation.stationCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                      [] ->
                        createEntriesInFareTables validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id
          else do
            -- Process route stations
            forM_ quote.routeStations $ \routeStation -> do
              let mbStartStopCode = find (\station -> station.stationType == Station.START) routeStation.routeStations <&> (.stationCode)
                  mbEndStopCode = find (\station -> station.stationType == Station.END) routeStation.routeStations <&> (.stationCode)
              case routeStation.routeFarePolicyId of
                Just farePolicyId ->
                  whenJust ((,) <$> mbStartStopCode <*> mbEndStopCode) $ \(startStopCode, endStopCode) ->
                    forM_ quote.categories $ \quoteCategory ->
                      upsertStopFare farePolicyId startStopCode endStopCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id
                Nothing ->
                  QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                    fareProducts@(_ : _) -> do
                      let farePolicyIds = map (.farePolicyId) fareProducts
                      whenJust ((,) <$> mbStartStopCode <*> mbEndStopCode) $ \(startStopCode, endStopCode) ->
                        forM_ quote.categories $ \quoteCategory ->
                          traverse_ (\farePolicyId -> upsertStopFare farePolicyId startStopCode endStopCode quoteCategory validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                    [] ->
                      createEntriesInFareTables validatedReq.search.merchantId validatedReq.search.merchantOperatingCityId quote quote.categories integratedBPPConfig.id

  logInfo $ "Fare cache refresh completed for search: " <> onSearchReq.transactionId
  where
    updateQuote integratedBppConfig quote = do
      stations <-
        forM quote.stations \station -> do
          stationCode <- OTPRest.getStopCodeFromProviderCode integratedBppConfig station.stationCode
          return $ station {stationCode = fromMaybe station.stationCode stationCode}
      return $ quote {stations = stations}

    cacheQuote (quote, quoteCategories) = do
      whenJust (find (\category -> category.category == ADULT) quoteCategories <&> (.price)) $ \price -> do
        let key =
              CachedQuote.FRFSCachedQuoteKey
                { CachedQuote.fromStationId = quote.fromStationCode,
                  CachedQuote.toStationId = quote.toStationCode,
                  CachedQuote.providerId = quote.providerId,
                  CachedQuote.quoteType = quote._type
                }
        CachedQuote.cacheByFRFSCachedQuoteKey key CachedQuote.FRFSCachedQuote {CachedQuote.price = price, CachedQuote.stationsJson = quote.stationsJson}

filterQuotes :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DIBC.IntegratedBPPConfig -> [(Quote.FRFSQuote, [FRFSQuoteCategory])] -> Maybe DJourneyLeg.JourneyLeg -> m (Maybe (Quote.FRFSQuote, [FRFSQuoteCategory]))
filterQuotes _ [] _ = return Nothing
filterQuotes _ _ Nothing = return Nothing
filterQuotes integratedBPPConfig quotesWithCategories (Just journeyLeg) = do
  filteredQuotesWithCategories <- case journeyLeg.liveVehicleAvailableServiceTypes of
    Just liveVehicleAvailableServiceTypes@(_ : _) -> do
      return $
        quotesWithCategories
          & filter
            ( \(quote, quoteCategories) ->
                maybe False (\serviceTier -> serviceTier.serviceTierType `elem` liveVehicleAvailableServiceTypes) (JourneyUtils.getServiceTierFromQuote quoteCategories quote)
                  && isRouteBasedQuote quote
            )
    _ -> return $ filter (isRouteBasedQuote . fst) quotesWithCategories
  let finalQuotesWithCategories = if null filteredQuotesWithCategories && (integratedBPPConfig.isTicketValidOnMultipleRoutes == Just True || isNothing journeyLeg.finalBoardedBusNumber) then quotesWithCategories else filteredQuotesWithCategories
  case finalQuotesWithCategories of
    [] -> return Nothing
    _ -> do
      case journeyLeg.mode of
        DTripTypes.Bus -> do
          mbRiderConfig <- QRC.findByMerchantOperatingCityId journeyLeg.merchantOperatingCityId Nothing
          let cfgMap = maybe (JourneyUtils.toCfgMap JourneyUtils.defaultBusTierSortingConfig) JourneyUtils.toCfgMap (mbRiderConfig >>= (.busTierSortingConfig))
          let serviceTierTypeFromQuote quote quoteCategories = JourneyUtils.getServiceTierFromQuote quoteCategories quote <&> (.serviceTierType)
          return $
            Just $
              minimumBy
                ( \(quote1, quoteCategories1) (quote2, quoteCategories2) ->
                    compare
                      (maybe maxBound (JourneyUtils.tierRank cfgMap) (serviceTierTypeFromQuote quote1 quoteCategories1))
                      (maybe maxBound (JourneyUtils.tierRank cfgMap) (serviceTierTypeFromQuote quote2 quoteCategories2))
                )
                finalQuotesWithCategories
        _ ->
          return $
            Just $
              minimumBy
                ( \(_, quoteCategories1) (_, quoteCategories2) ->
                    let mbAdultPrice1 = find (\category -> category.category == ADULT) quoteCategories1 <&> (.price)
                        mbAdultPrice2 = find (\category -> category.category == ADULT) quoteCategories2 <&> (.price)
                     in compare (maybe 0 (.amount) mbAdultPrice1) (maybe 0 (.amount) mbAdultPrice2)
                )
                finalQuotesWithCategories
  where
    isRouteBasedQuote quote =
      -- TODO :: Can be used across all, but as we don't want to break others we are doing this only for ONDC
      case integratedBPPConfig.providerConfig of
        DIBC.ONDC config ->
          (config.routeBasedQuoteSelection /= Just True)
            || maybe
              True
              ( \(routeStationsJson :: [API.FRFSRouteStationsAPI], firstRouteDetail) ->
                  any (\route -> Just route.code == firstRouteDetail.routeCode) routeStationsJson
              )
              ((,) <$> (decodeFromText =<< quote.routeStationsJson) <*> (listToMaybe journeyLeg.routeDetails))
        _ -> True

mkQuotes :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasShortDurationRetryCfg r c) => DOnSearch -> ValidatedDOnSearch -> DQuote -> m (Quote.FRFSQuote, [FRFSQuoteCategory])
mkQuotes dOnSearch ValidatedDOnSearch {..} DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity search
  startStation <- OTPRest.getStationByGtfsIdAndStopCode dStartStation.stationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dStartStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  endStation <- OTPRest.getStationByGtfsIdAndStopCode dEndStation.stationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dEndStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  let stationsJSON = stations & map (castStationToAPI integratedBPPConfig.id) & encodeToText
  let routeStationsJSON = routeStations & map (castRouteStationToAPI integratedBPPConfig.id) & encodeToText
  uid <- generateGUID
  now <- getCurrentTime
  let mbAdultPrice = find (\category -> category.category == ADULT) categories <&> (.price)
      (discountedTickets, eventDiscountAmount) =
        case mbAdultPrice of
          Just adultPrice -> SFU.getDiscountInfo isEventOngoing mbFreeTicketInterval mbMaxFreeTicketCashback adultPrice search.quantity ticketsBookedInEvent
          Nothing -> (Nothing, Nothing)
  let validTill = fromMaybe (addUTCTime (intToNominalDiffTime 900) now) dOnSearch.validTill -- If validTill is not present, set it to 15 minutes from now
      frfsQuote =
        Quote.FRFSQuote
          { Quote._type = _type,
            Quote.bppItemId,
            Quote.bppSubscriberId = dOnSearch.bppSubscriberId,
            Quote.bppSubscriberUrl = dOnSearch.bppSubscriberUrl,
            Quote.fromStationCode = startStation.code,
            Quote.toStationCode = endStation.code,
            Quote.id = uid,
            Quote.providerDescription = dOnSearch.providerDescription,
            Quote.providerId = dOnSearch.providerId,
            Quote.providerName = dOnSearch.providerName,
            Quote.riderId = search.riderId,
            Quote.searchId = search.id,
            Quote.stationsJson = stationsJSON,
            Quote.routeStationsJson = Just routeStationsJSON,
            Quote.validTill,
            Quote.vehicleType,
            Quote.merchantId = search.merchantId,
            Quote.merchantOperatingCityId = search.merchantOperatingCityId,
            Quote.partnerOrgId = search.partnerOrgId,
            Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
            Quote.createdAt = now,
            Quote.updatedAt = now,
            Quote.integratedBppConfigId = search.integratedBppConfigId,
            Quote.multimodalSearchRequestId = search.multimodalSearchRequestId,
            Quote.busLocationData = search.busLocationData,
            Quote.fromStationAddress = search.fromStationAddress,
            Quote.fromStationName = search.fromStationName,
            Quote.fromStationPoint = search.fromStationPoint,
            Quote.toStationAddress = search.toStationAddress,
            Quote.toStationName = search.toStationName,
            Quote.toStationPoint = search.toStationPoint,
            Quote.vehicleNumber = search.vehicleNumber,
            bppDelayedInterest = readMaybe . T.unpack =<< dOnSearch.bppDelayedInterest,
            oldCacheDump = Nothing,
            ..
          }

  frfsQuoteCategories <-
    forM categories $ \category -> do
      quoteCategoryId <- generateGUID
      ticketCategoryMetadataConfig' <- QFRFSTicketCategoryMetadataConfig.findByCategoryVehicleAndCity category.category vehicleType search.merchantOperatingCityId
      return
        FRFSQuoteCategory
          { id = quoteCategoryId,
            category = category.category,
            quoteId = uid,
            bppItemId = category.bppItemId,
            price = category.price, -- Single Ticket Price
            offeredPrice = category.offeredPrice, -- Single Ticket Offered Price (Should be less than or equal to price)
            finalPrice = Nothing,
            categoryMeta = TFQC.mkQuoteCategoryMetadata (ticketCategoryMetadataConfig' <&> (.code)) (ticketCategoryMetadataConfig' <&> (.title)) (ticketCategoryMetadataConfig' <&> (.description)) (ticketCategoryMetadataConfig' <&> (.tnc)),
            merchantId = search.merchantId,
            merchantOperatingCityId = search.merchantOperatingCityId,
            selectedQuantity = if category.category == ADULT then search.quantity else 0, -- To Handle Partner Org
            createdAt = now,
            updatedAt = now
          }

  return (frfsQuote, frfsQuoteCategories)

getStartStation :: [DStation] -> Maybe DStation
getStartStation = find (\station -> station.stationType == Station.START)

getEndStation :: [DStation] -> Maybe DStation
getEndStation = find (\station -> station.stationType == Station.END)

castStationToAPI :: Id DIBC.IntegratedBPPConfig -> DStation -> API.FRFSStationAPI
castStationToAPI integratedBppConfigId DStation {..} =
  API.FRFSStationAPI
    { API.address = Nothing,
      API.code = stationCode,
      API.parentStopCode = Nothing,
      API.routeCodes = Nothing,
      API.color = Nothing,
      API.lat = stationLat,
      API.lon = stationLon,
      API.name = Just stationName,
      API.stationType = Just stationType,
      API.sequenceNum = stopSequence,
      API.distance = Nothing,
      API.towards = Nothing,
      API.timeTakenToTravelUpcomingStop = Nothing,
      API.integratedBppConfigId = integratedBppConfigId
    }

castRouteStationToAPI :: Id DIBC.IntegratedBPPConfig -> DRouteStation -> API.FRFSRouteStationsAPI
castRouteStationToAPI integratedBppConfigId DRouteStation {..} =
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
      API.stations = map (castStationToAPI integratedBppConfigId) routeStations
    }

castVehicleServiceTierAPI :: DVehicleServiceTier -> API.FRFSVehicleServiceTierAPI
castVehicleServiceTierAPI DVehicleServiceTier {..} =
  API.FRFSVehicleServiceTierAPI
    { _type = serviceTierType,
      providerCode = serviceTierProviderCode,
      description = serviceTierDescription,
      longName = serviceTierLongName,
      shortName = serviceTierShortName,
      isAirConditioned = isAirConditioned
    }

updateQuotes :: ((Quote.FRFSQuote, [FRFSQuoteCategory]), (Quote.FRFSQuote, [FRFSQuoteCategory])) -> Quote.FRFSQuote
updateQuotes ((quotesFromCache, quotesFromCacheCategories), (quotesFromOnSearch, quotesFromOnSearchCategories)) = do
  let fareParametersWithCacheCategories = SFU.mkFareParameters (SFU.mkCategoryPriceItemFromQuoteCategories quotesFromCacheCategories)
      fareParametersWithOnSearchCategories = SFU.mkFareParameters (SFU.mkCategoryPriceItemFromQuoteCategories quotesFromOnSearchCategories)
      singleAdultTicketPriceWithCacheCategories = find (\category -> category.categoryType == ADULT) fareParametersWithCacheCategories.priceItems <&> (.unitPrice)
      singleAdultTicketPriceWithOnSearchCategories = find (\category -> category.categoryType == ADULT) fareParametersWithOnSearchCategories.priceItems <&> (.unitPrice)
      isQuoteChanged = not $ singleAdultTicketPriceWithCacheCategories == singleAdultTicketPriceWithOnSearchCategories && quotesFromCache.stationsJson == quotesFromOnSearch.stationsJson
  let oldCacheDump =
        if isQuoteChanged
          then singleAdultTicketPriceWithCacheCategories <&> (\singleAdultTicketPrice -> toJsonText FRFSCachedQuote {price = singleAdultTicketPrice, stationsJson = quotesFromCache.stationsJson})
          else Nothing

  Quote.FRFSQuote
    { Quote._type = quotesFromCache._type,
      Quote.bppItemId = quotesFromOnSearch.bppItemId,
      Quote.bppSubscriberId = quotesFromOnSearch.bppSubscriberId,
      Quote.bppSubscriberUrl = quotesFromOnSearch.bppSubscriberUrl,
      Quote.fromStationCode = quotesFromCache.fromStationCode,
      Quote.id = quotesFromCache.id,
      Quote.providerDescription = quotesFromOnSearch.providerDescription,
      Quote.providerId = quotesFromCache.providerId,
      Quote.providerName = quotesFromCache.providerName,
      Quote.riderId = quotesFromCache.riderId,
      Quote.searchId = quotesFromCache.searchId,
      Quote.stationsJson = quotesFromCache.stationsJson,
      Quote.routeStationsJson = quotesFromOnSearch.routeStationsJson,
      Quote.toStationCode = quotesFromCache.toStationCode,
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
      Quote.fareDetails = quotesFromOnSearch.fareDetails,
      Quote.eventDiscountAmount = quotesFromOnSearch.eventDiscountAmount,
      Quote.integratedBppConfigId = quotesFromOnSearch.integratedBppConfigId,
      Quote.discountedTickets = quotesFromOnSearch.discountedTickets,
      Quote.multimodalSearchRequestId = quotesFromOnSearch.multimodalSearchRequestId,
      Quote.busLocationData = quotesFromOnSearch.busLocationData,
      Quote.fromStationAddress = quotesFromOnSearch.fromStationAddress,
      Quote.fromStationName = quotesFromOnSearch.fromStationName,
      Quote.fromStationPoint = quotesFromOnSearch.fromStationPoint,
      Quote.toStationAddress = quotesFromOnSearch.toStationAddress,
      Quote.toStationName = quotesFromOnSearch.toStationName,
      Quote.toStationPoint = quotesFromOnSearch.toStationPoint,
      Quote.vehicleNumber = quotesFromOnSearch.vehicleNumber
    }
  where
    toJsonText :: FRFSCachedQuote -> Text
    toJsonText cachedQuote = toStrict $ decodeUtf8 $ encode cachedQuote

updateQuoteCategoriesFromOnSearch ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ((Quote.FRFSQuote, [FRFSQuoteCategory]), (Quote.FRFSQuote, [FRFSQuoteCategory])) ->
  m ()
updateQuoteCategoriesFromOnSearch ((cachedQuote, cachedCats), (_onSearchQuote, onSearchCats)) = do
  now <- getCurrentTime
  -- Update existing cached categories with BPP data (match by category type)
  for_ cachedCats $ \cachedCat -> do
    let mbOnSearchCat = find (\c -> c.category == cachedCat.category && c.bppItemId == cachedCat.bppItemId) onSearchCats
    whenJust mbOnSearchCat $ \onSearchCat -> do
      let updatedCat =
            cachedCat
              { price = onSearchCat.price,
                offeredPrice = onSearchCat.offeredPrice,
                bppItemId = onSearchCat.bppItemId,
                categoryMeta = onSearchCat.categoryMeta,
                updatedAt = now
              }
      QFRFSQuoteCategory.updateByPrimaryKey updatedCat
  -- Create new categories from BPP that don't exist in cache (same quote id so they stay linked)
  let cachedCategories = map (.category) cachedCats
  for_ onSearchCats $ \onSearchCat ->
    when (onSearchCat.category `notElem` cachedCategories) $ do
      newId <- generateGUID
      let newCategory =
            onSearchCat
              { id = newId,
                quoteId = cachedQuote.id,
                merchantId = cachedQuote.merchantId,
                merchantOperatingCityId = cachedQuote.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QFRFSQuoteCategory.create newCategory

-- | Pairs cached quotes with BPP quotes by quote type (same order: SingleJourney first, then ReturnJourney).
-- When BPP sends more quotes than we have in cache, the extra BPP quotes are returned as 'newQuotesFromBpp'
-- so they can be created. When lengths or types don't match for pairing, we still pair what we can and
-- treat the rest as new from BPP (no throw).
matchQuotesForUpsert ::
  [(Quote.FRFSQuote, [FRFSQuoteCategory])] ->
  [(Quote.FRFSQuote, [FRFSQuoteCategory])] ->
  ( [((Quote.FRFSQuote, [FRFSQuoteCategory]), (Quote.FRFSQuote, [FRFSQuoteCategory]))],
    [(Quote.FRFSQuote, [FRFSQuoteCategory])]
  )
matchQuotesForUpsert quotesFromCacheWithQuoteCategories quotesFromOnSearchWithQuoteCategories =
  let sortedCache = sortBy (comparing (Down . Quote._type . fst)) quotesFromCacheWithQuoteCategories
      sortedBpp = sortBy (comparing (Down . Quote._type . fst)) quotesFromOnSearchWithQuoteCategories
      zipped = zip sortedCache sortedBpp
      (validPairs, mismatchedOrExtra) =
        partition
          ( \((cachedQuote, _), (bppQuote, _)) ->
              cachedQuote._type == bppQuote._type
          )
          zipped
      -- BPP quotes that were paired but had type mismatch, or BPP quotes beyond cache length
      newQuotesFromBpp = map snd mismatchedOrExtra ++ drop (length sortedCache) sortedBpp
   in (validPairs, newQuotesFromBpp)

verifyAndZipQuotes :: (MonadFlow m) => [(Quote.FRFSQuote, [FRFSQuoteCategory])] -> [(Quote.FRFSQuote, [FRFSQuoteCategory])] -> m [((Quote.FRFSQuote, [FRFSQuoteCategory]), (Quote.FRFSQuote, [FRFSQuoteCategory]))]
verifyAndZipQuotes quotesFromCacheWithQuoteCategories quotesFromOnSearchWithQuoteCategories = do
  let quotesFromCache = map fst quotesFromCacheWithQuoteCategories
      quotesFromOnSearch = map fst quotesFromOnSearchWithQuoteCategories
  if length quotesFromCache /= length quotesFromOnSearch
    then throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
    else case (quotesFromCacheWithQuoteCategories, quotesFromOnSearchWithQuoteCategories) of
      ([fq1@(q1, _)], [fq2@(q2, _)]) ->
        if q1._type == q2._type
          then return [(fq1, fq2)]
          else throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
      _ -> do
        let isBothQuotesValid = verifyQuote quotesFromCache && verifyQuote quotesFromOnSearch
        if isBothQuotesValid
          then do
            let sortedQ1 = sortBy (comparing (Down . Quote._type . fst)) quotesFromCacheWithQuoteCategories
            let sortedQ2 = sortBy (comparing (Down . Quote._type . fst)) quotesFromOnSearchWithQuoteCategories
            return (zip sortedQ1 sortedQ2)
          else throwError $ CachedFRFSQuoteAnomaly (show quotesFromCache) (show quotesFromOnSearch)
  where
    verifyQuote quotes =
      length quotes == 2
        && any (\q -> q._type == Quote.SingleJourney) quotes
        && any (\q -> q._type == Quote.ReturnJourney) quotes

createStopFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FRFSFarePolicy.FRFSFarePolicy -> Text -> Text -> DCategory -> Id Merchant -> Id MerchantOperatingCity -> Id DIBC.IntegratedBPPConfig -> m ()
createStopFare farePolicyId startStopCode endStopCode dCategory merchantId merchantOperatingCityId integratedBppConfigId = do
  now <- getCurrentTime
  let stopFare =
        StopFare.StopFare
          { farePolicyId,
            startStopCode = startStopCode,
            endStopCode = endStopCode,
            amount = dCategory.price.amount,
            offeredAmount = Just dCategory.offeredPrice.amount,
            currency = dCategory.price.currency,
            category = dCategory.category,
            bppItemId = Just dCategory.bppItemId,
            merchantId,
            merchantOperatingCityId,
            integratedBppConfigId,
            createdAt = now,
            updatedAt = now
          }
  QRSF.create stopFare

upsertStopFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FRFSFarePolicy.FRFSFarePolicy -> Text -> Text -> DCategory -> Id Merchant -> Id MerchantOperatingCity -> Id DIBC.IntegratedBPPConfig -> m ()
upsertStopFare farePolicyId startStopCode endStopCode dCategory merchantId merchantOperatingCityId integratedBppConfigId = do
  existingFare <- QRSF.findByPrimaryKey dCategory.category endStopCode farePolicyId startStopCode
  case existingFare of
    Just _ -> QRSF.updateFareByStopCodes dCategory.price.amount farePolicyId startStopCode endStopCode dCategory.category
    Nothing -> createStopFare farePolicyId startStopCode endStopCode dCategory merchantId merchantOperatingCityId integratedBppConfigId

createEntriesInFareTables :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> DQuote -> [DCategory] -> Id DIBC.IntegratedBPPConfig -> m ()
createEntriesInFareTables merchantId merchantOperatingCityId quote categories integratedBppConfigId = do
  fareProductId <- generateGUID
  farePolicyId <- generateGUID
  now <- getCurrentTime
  let routeCode = quote.routeCode
      startStopCode = find (\station -> station.stationType == Station.START) quote.stations <&> (.stationCode)
      endStopCode = find (\station -> station.stationType == Station.END) quote.stations <&> (.stationCode)
  let farePolicy =
        FRFSFarePolicy.FRFSFarePolicy
          { id = farePolicyId,
            _type = FRFSFarePolicy.MatrixBased,
            applicableDiscountIds = [],
            description = "Matrix Based Fare Policy",
            cessCharge = Nothing,
            merchantId,
            merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  let serviceTierType =
        case quote.routeStations of
          (routeStation : _) -> maybe Spec.ORDINARY (.serviceTierType) (routeStation.routeServiceTier)
          [] -> Spec.ORDINARY

  (vehicleServiceTierId, vehicleServiceTier) <- do
    CQVSR.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType merchantOperatingCityId integratedBppConfigId >>= \case
      Just vsc -> do
        return (vsc.id, Nothing)
      Nothing -> do
        id <- generateGUID
        return
          ( id,
            Just $
              FRFSVehicleServiceTier.FRFSVehicleServiceTier
                { id,
                  _type = serviceTierType,
                  providerCode = show serviceTierType,
                  description = show serviceTierType,
                  shortName = show quote.vehicleType,
                  longName = show quote.vehicleType,
                  isAirConditioned = Just False,
                  integratedBppConfigId,
                  merchantId,
                  merchantOperatingCityId,
                  trainType = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
          )

  let frfsRouteFareProduct =
        FRFSRouteFareProduct.FRFSRouteFareProduct
          { id = fareProductId,
            routeCode,
            vehicleType = quote.vehicleType,
            farePolicyId,
            vehicleServiceTierId = vehicleServiceTierId,
            merchantId,
            merchantOperatingCityId,
            timeBounds = DTB.Unbounded,
            integratedBppConfigId,
            createdAt = now,
            updatedAt = now
          }
  whenJust vehicleServiceTier $ \vsc -> do
    QVSR.create vsc
  QFRFP.create frfsRouteFareProduct
  QFFP.create farePolicy

  -- Create StopFare entry for EACH category
  forM_ categories $ \quoteCategory -> do
    let routeStopFare =
          StopFare.StopFare
            { farePolicyId,
              startStopCode = fromMaybe "" startStopCode,
              endStopCode = fromMaybe "" endStopCode,
              amount = quoteCategory.price.amount,
              offeredAmount = Just quoteCategory.offeredPrice.amount,
              currency = quoteCategory.price.currency,
              category = quoteCategory.category,
              bppItemId = Just quoteCategory.bppItemId,
              merchantId,
              merchantOperatingCityId,
              integratedBppConfigId,
              createdAt = now,
              updatedAt = now
            }
    QRSF.create routeStopFare

discoveryOnSearch ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  DiscoveryOnSearchReq ->
  m ()
discoveryOnSearch _discoveryReq = pure ()
