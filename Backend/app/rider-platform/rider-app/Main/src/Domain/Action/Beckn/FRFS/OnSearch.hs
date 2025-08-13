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
import Data.Aeson
import Data.List (sortBy)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Extra.FRFSCachedQuote as CachedQuote
import qualified Domain.Types.FRFSFarePolicy as FRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSRouteFareProduct as FRFSRouteFareProduct
import qualified Domain.Types.FRFSSearch as Search
import qualified Domain.Types.FRFSVehicleServiceTier as FRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType as Station
import qualified Domain.Types.StopFare as StopFare
import EulerHS.Prelude (comparing, toStrict)
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Types as JourneyTypes
import qualified SharedLogic.CreateFareForMultiModal as SLCF
import qualified SharedLogic.FRFSUtils as SFU
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSFarePolicy as QFFP
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSRouteFareProduct as QFRFP
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSVehicleServiceTier as QVSR
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.StopFare as QRSF
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
    routeCode :: Text,
    price :: Price,
    childPrice :: Maybe Price,
    vehicleType :: Spec.VehicleCategory,
    routeStations :: [DRouteStation],
    stations :: [DStation],
    discounts :: [DDiscount],
    fareDetails :: Maybe Quote.FRFSFareDetails,
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
    routeColor :: Maybe Text,
    routeFarePolicyId :: Maybe (Id FRFSFarePolicy.FRFSFarePolicy)
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
    CacheFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  DOnSearch ->
  ValidatedDOnSearch ->
  m ()
onSearch onSearchReq validatedReq = do
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
  filteredQuotes <-
    if validatedReq.search.vehicleType /= Spec.BUS
      then pure onSearchReq.quotes
      else do
        routeCodes <- mapM (\routeCode -> OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)) (catMaybes ((map (.routeCode) (maybe [] (.routeDetails) mbJourneyLeg)) <> [validatedReq.search.routeCode]))
        pure $ case routeCodes of
          [] -> onSearchReq.quotes
          routesCodes' -> filter (\quote -> quote.routeCode `elem` map (.code) routesCodes') onSearchReq.quotes
  quotes <- traverse (mkQuotes onSearchReq validatedReq) filteredQuotes
  traverse_ cacheQuote quotes
  if null quotesCreatedByCache
    then QQuote.createMany quotes
    else do
      zippedQuotes <- verifyAndZipQuotes quotesCreatedByCache quotes
      let updatedQuotes = map updateQuotes zippedQuotes
      for_ updatedQuotes \quote -> QQuote.updateCachedQuoteByPrimaryKey quote
  let search = validatedReq.search
  mbRequiredQuote <- filterQuotes quotes mbJourneyLeg
  case mbRequiredQuote of
    Just requiredQuote -> do
      void $ SLCF.createFares search.id.getId search.journeyLegInfo (QSearch.updatePricingId validatedReq.search.id (Just requiredQuote.id.getId))
      QJourneyLeg.updateEstimatedFaresBySearchId (Just requiredQuote.price.amount) (Just requiredQuote.price.amount) (Just validatedReq.search.id.getId)
    Nothing -> do
      whenJust validatedReq.search.journeyLegInfo $ \_journeyLegInfo -> do
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
                  routeStopFares@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) routeStopFares
                    traverse_ (\fp -> QRSF.updateFareByStopCodes quote.price.amount fp dStartStation.stationCode dEndStation.stationCode) farePolicyIds
                  [] -> do
                    QFRFP.findAllByIntegratedBPPConfigId integratedBPPConfig.id >>= \case
                      fareProducts@(_ : _) -> do
                        let farePolicyIds = map (.farePolicyId) fareProducts
                        traverse_ (\farePolicyId -> createStopFare farePolicyId dStartStation.stationCode dEndStation.stationCode quote search.merchantId search.merchantOperatingCityId integratedBPPConfig.id) farePolicyIds
                      [] -> do
                        createEntriesInFareTables search.merchantId search.merchantOperatingCityId quote integratedBPPConfig.id
              else do
                QFRFP.findByRouteCode quote.routeCode integratedBPPConfig.id >>= \case
                  fareProducts@(_ : _) -> do
                    let farePolicyIds = map (.farePolicyId) fareProducts
                    farePolicies <- QFFP.findAllByIds farePolicyIds
                    let filteredFarePolicies = filter (\fp -> fp._type == FRFSFarePolicy.MatrixBased) farePolicies
                    traverse_ (\fp -> QRSF.updateFareByStopCodes quote.price.amount fp.id dStartStation.stationCode dEndStation.stationCode) filteredFarePolicies
                  [] -> do
                    createEntriesInFareTables search.merchantId search.merchantOperatingCityId quote integratedBPPConfig.id
          else do
            forM_ quote.routeStations $ \routeStation -> do
              let price = routeStation.routePrice.amount
                  mbStartStopCode = find (\station -> station.stationType == Station.START) routeStation.routeStations <&> (.stationCode)
                  mbEndStopCode = find (\station -> station.stationType == Station.END) routeStation.routeStations <&> (.stationCode)
              whenJust ((,,) <$> routeStation.routeFarePolicyId <*> mbStartStopCode <*> mbEndStopCode) $ \(farePolicyId, startStopCode, endStopCode) ->
                QRSF.updateFareByStopCodes price farePolicyId startStopCode endStopCode
  return ()
  where
    cacheQuote quote = do
      let key =
            CachedQuote.FRFSCachedQuoteKey
              { CachedQuote.fromStationId = quote.fromStationCode,
                CachedQuote.toStationId = quote.toStationCode,
                CachedQuote.providerId = quote.providerId,
                CachedQuote.quoteType = quote._type
              }
      CachedQuote.cacheByFRFSCachedQuoteKey key CachedQuote.FRFSCachedQuote {CachedQuote.price = quote.price, CachedQuote.stationsJson = quote.stationsJson}
    createStopFare farePolicyId startStopCode endStopCode quote merchantId merchantOperatingCityId integratedBppConfigId = do
      now <- getCurrentTime
      let stopFare =
            StopFare.StopFare
              { farePolicyId,
                startStopCode = startStopCode,
                endStopCode = endStopCode,
                amount = quote.price.amount,
                currency = quote.price.currency,
                merchantId,
                merchantOperatingCityId,
                integratedBppConfigId,
                createdAt = now,
                updatedAt = now
              }
      QRSF.create stopFare

filterQuotes :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => [Quote.FRFSQuote] -> Maybe DJourneyLeg.JourneyLeg -> m (Maybe Quote.FRFSQuote)
filterQuotes [] _ = return Nothing
filterQuotes quotes (Just journeyLeg) = do
  filteredQuotes <- case journeyLeg.serviceTypes of
    Just serviceTypes -> do
      return $ quotes & filter (maybe False (\serviceTier -> serviceTier.serviceTierType `elem` serviceTypes) . JourneyTypes.getServiceTierFromQuote)
    Nothing -> return quotes
  return $ Just $ minimumBy (\quote1 quote2 -> compare quote1.price.amount.getHighPrecMoney quote2.price.amount.getHighPrecMoney) (if null filteredQuotes then quotes else filteredQuotes)
filterQuotes _ Nothing = return Nothing

mkQuotes :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasShortDurationRetryCfg r c) => DOnSearch -> ValidatedDOnSearch -> DQuote -> m Quote.FRFSQuote
mkQuotes dOnSearch ValidatedDOnSearch {..} DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity search
  startStation <- OTPRest.getStationByGtfsIdAndStopCode dStartStation.stationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dStartStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  endStation <- OTPRest.getStationByGtfsIdAndStopCode dEndStation.stationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> dEndStation.stationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  let stationsJSON = stations & map (castStationToAPI integratedBPPConfig.id) & encodeToText
  let routeStationsJSON = routeStations & map (castRouteStationToAPI integratedBPPConfig.id) & encodeToText
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
        Quote.fromStationCode = startStation.code,
        Quote.toStationCode = endStation.code,
        Quote.id = uid,
        Quote.price,
        Quote.childPrice,
        Quote.estimatedPrice = Just price,
        Quote.providerDescription = dOnSearch.providerDescription,
        Quote.providerId = dOnSearch.providerId,
        Quote.providerName = dOnSearch.providerName,
        Quote.quantity = search.quantity,
        Quote.riderId = search.riderId,
        Quote.searchId = search.id,
        Quote.stationsJson = stationsJSON,
        Quote.routeStationsJson = Just routeStationsJSON,
        Quote.discountsJson = Just discountsJSON,
        Quote.validTill,
        Quote.vehicleType,
        Quote.merchantId = search.merchantId,
        Quote.merchantOperatingCityId = search.merchantOperatingCityId,
        Quote.partnerOrgId = search.partnerOrgId,
        Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
        Quote.createdAt = now,
        Quote.updatedAt = now,
        Quote.integratedBppConfigId = search.integratedBppConfigId,
        Quote.childTicketQuantity = Nothing,
        bppDelayedInterest = readMaybe . T.unpack =<< dOnSearch.bppDelayedInterest,
        oldCacheDump = Nothing,
        ..
      }

getStartStation :: [DStation] -> Maybe DStation
getStartStation = find (\station -> station.stationType == Station.START)

getEndStation :: [DStation] -> Maybe DStation
getEndStation = find (\station -> station.stationType == Station.END)

castStationToAPI :: Id DIBC.IntegratedBPPConfig -> DStation -> API.FRFSStationAPI
castStationToAPI integratedBppConfigId DStation {..} =
  API.FRFSStationAPI
    { API.address = Nothing,
      API.code = stationCode,
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
      Quote.fromStationCode = quotesFromCache.fromStationCode,
      Quote.id = quotesFromCache.id,
      Quote.price = quotesFromOnSearch.price,
      Quote.childPrice = quotesFromOnSearch.childPrice,
      Quote.estimatedPrice = quotesFromOnSearch.estimatedPrice,
      Quote.providerDescription = quotesFromOnSearch.providerDescription,
      Quote.providerId = quotesFromCache.providerId,
      Quote.providerName = quotesFromCache.providerName,
      Quote.quantity = quotesFromOnSearch.quantity,
      Quote.riderId = quotesFromCache.riderId,
      Quote.searchId = quotesFromCache.searchId,
      Quote.stationsJson = quotesFromCache.stationsJson,
      Quote.routeStationsJson = quotesFromOnSearch.routeStationsJson,
      Quote.discountsJson = quotesFromOnSearch.discountsJson,
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
      Quote.childTicketQuantity = quotesFromOnSearch.childTicketQuantity
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

createEntriesInFareTables :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> DQuote -> Id DIBC.IntegratedBPPConfig -> m ()
createEntriesInFareTables merchantId merchantOperatingCityId quote integratedBppConfigId = do
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
  let routeStopFare =
        StopFare.StopFare
          { farePolicyId,
            startStopCode = fromMaybe "" startStopCode,
            endStopCode = fromMaybe "" endStopCode,
            amount = quote.price.amount,
            currency = quote.price.currency,
            merchantId,
            merchantOperatingCityId,
            integratedBppConfigId,
            createdAt = now,
            updatedAt = now
          }
  (vehicleServiceTierId, vehicleServiceTier) <- do
    QVSR.findByServiceTierAndMerchantOperatingCityId Spec.ORDINARY merchantOperatingCityId >>= \case
      Just vsc -> return (vsc.id, Nothing)
      Nothing -> do
        id <- generateGUID
        return
          ( id,
            Just $
              FRFSVehicleServiceTier.FRFSVehicleServiceTier
                { id,
                  _type = Spec.ORDINARY,
                  providerCode = "ORDINARY",
                  description = "ORDINARY",
                  shortName = show quote.vehicleType,
                  longName = show quote.vehicleType,
                  merchantId,
                  merchantOperatingCityId,
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
  QRSF.create routeStopFare
