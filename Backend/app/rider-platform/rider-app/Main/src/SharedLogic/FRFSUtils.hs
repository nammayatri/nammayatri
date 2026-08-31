{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSUtils
  ( module SharedLogic.FRFSUtils,
    module Reexport,
  )
where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra (mapMaybeM)
import Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, nub, sort, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Types.AadhaarVerification as DAadhaarVerification
import Domain.Types.BecknConfig
import qualified Domain.Types.Extra.VendorSplitDetails as VendorSplitDetails
import qualified Domain.Types.FRFSBookingGroup as DFRFSBookingGroup
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategorySpec as FRFSCategorySpec
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSRecon as Recon
import Domain.Types.FRFSRouteFareProduct
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.FRFSTicketBookingPaymentCategory as DTBPC
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig as DFRFSTicketCategoryMetadataConfig
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicketStatus
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Domain.Types.RouteTripMapping as DRTM
import qualified Domain.Types.Seat as DSeat
import qualified Domain.Types.Station as Station
import qualified Domain.Types.VendorSplitDetails as VendorSplitDetails
import EulerHS.Prelude (comparing, concatMapM, (+||), (<|>), (||+))
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types ()
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSFareCalculator as Reexport
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.OfferSegment as SOfferSegment
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSGtfsStageFare as QFRFSGtfsStageFare
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import Storage.CachedQueries.Merchant.MultiModalBus (utcToIST)
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import qualified Storage.Queries.FRFSGtfsStageFare as QQFRFSGtfsStageFare
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import Storage.Queries.FRFSStageFare as QFRFSStageFare
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBookingPaymentCategory as QFRFSTicketBookingPaymentCategory
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.JourneyLeg as QJL
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.RouteTripMapping as QRouteTripMapping
import Storage.Queries.StopFare as QRouteStopFare
import qualified Storage.Queries.VendorSplitDetails as QVendorSplitDetails
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Payment as Payment
import qualified Tools.Wallet as TWallet

frfsGtfsCacheKey :: Text -> Text
frfsGtfsCacheKey ibcId = "frfs:gtfs:" <> ibcId

frfsGtfsPagesKey :: Text -> Text
frfsGtfsPagesKey ibcId = "frfs:gtfs:pages:" <> ibcId

frfsGtfsCacheTtlSec :: Int
frfsGtfsCacheTtlSec = 3600

adjustCfgMapForPreferredTier ::
  Ord Spec.ServiceTierType =>
  Maybe Spec.ServiceTierType ->
  M.Map Spec.ServiceTierType Int ->
  M.Map Spec.ServiceTierType Int
adjustCfgMapForPreferredTier mbPreferred cfgMap =
  case mbPreferred of
    Just pref ->
      let cfgMapInc = M.map (+ 1) cfgMap
       in M.insert pref 0 cfgMapInc
    Nothing -> cfgMap

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case (integrationBPPConfig.providerName, integrationBPPConfig.providerConfig) of
    (Just name, _) -> name
    (_, DIBC.CMRL _) -> "Chennai Metro Rail Limited"
    (_, DIBC.CMRLV2 _) -> "Chennai Metro Rail Limited v2"
    (_, DIBC.EBIX _) -> "Kolkata Buses"
    (_, DIBC.DIRECT _) -> "Direct Multimodal Services"
    (_, DIBC.ONDC _) -> "ONDC Services"
    (_, DIBC.CRIS _) -> "CRIS Subway"

getQREncoding :: DIBC.IntegratedBPPConfig -> Maybe DIBC.QREncoding
getQREncoding integratedBPPConfig = case integratedBPPConfig.providerConfig of
  DIBC.ONDC ondcConfig -> ondcConfig.qrEncoding
  _ -> Nothing

mkTicketAPI :: Maybe DIBC.QREncoding -> DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI qrEncoding DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- maybe (pure Nothing) (\pOrgId -> CQPOS.findByStationCodeAndPOrgId code pOrgId |<|>| CQPOS.findByStationCodeAndPOrgId id.getId pOrgId) mbPOrgId
  let pOrgStationName = pOrgStation <&> (.name)
  pure $ APITypes.FRFSStationAPI {name = Just $ fromMaybe name pOrgStationName, routeCodes = Nothing, stationType = Nothing, color = Nothing, routeDetails = Nothing, sequenceNum = Nothing, distance = Nothing, towards = Nothing, timeTakenToTravelUpcomingStop = Nothing, ..}

mkTBPStatusAPI :: DTBP.FRFSTicketBookingPaymentStatus -> APITypes.FRFSBookingPaymentStatusAPI
mkTBPStatusAPI = \case
  DTBP.PENDING -> APITypes.PENDING
  DTBP.SUCCESS -> APITypes.SUCCESS
  DTBP.FAILED -> APITypes.FAILURE
  DTBP.REFUND_PENDING -> APITypes.REFUND_PENDING
  DTBP.REFUNDED -> APITypes.REFUNDED
  DTBP.REFUND_FAILED -> APITypes.REFUND_FAILED
  DTBP.REFUND_INITIATED -> APITypes.REFUND_INITIATED

safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail [_] = Nothing
safeTail xs = Just (last xs)

mkFRFSConfigAPI :: Config.FRFSConfig -> APITypes.FRFSConfigAPIRes
mkFRFSConfigAPI Config.FRFSConfig {..} = do
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, cityId = merchantOperatingCityId, ..}

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r c) => Maybe (Id DPO.PartnerOrganization) -> DIBC.IntegratedBPPConfig -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId integratedBPPConfig stationAPI = do
  station <- B.runInReplica $ OTPRest.getStationByGtfsIdAndStopCode stationAPI.code integratedBPPConfig >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "and integratedBPPConfigId: " +|| integratedBPPConfig.id.getId ||+ "")
  mkPOrgStationAPIRes station mbPOrgId

data FRFSTicketCategoryDynamic = FRFSTicketCategoryDynamic
  { aadhaarData :: Maybe DAadhaarVerification.AadhaarVerification,
    ticketCategories :: [DFRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data RouteStopInfo = RouteStopInfo
  { route :: Route.Route,
    startStopCode :: Text,
    endStopCode :: Text,
    totalStops :: Maybe Int,
    stops :: Maybe [RouteStopMapping.RouteStopMapping],
    travelTime :: Maybe Seconds
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => Text -> Text -> IntegratedBPPConfig -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig = IM.withInMemCache ["POSSIBLEROUTES", startStationCode, endStationCode, integratedBPPConfig.id.getId] 7200 $ do
  routesWithStop <- OTPRest.getRouteStopMappingByStopCode startStationCode integratedBPPConfig
  let routeCodes = nub $ map (.routeCode) routesWithStop
  routeStops <-
    concatMapM
      (\routeCode -> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig)
      routeCodes
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) $ sortBy (compare `on` (.routeCode)) serviceableStops
      possibleRoutes =
        nub $
          catMaybes $
            map
              ( \stops ->
                  let stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) stops
                      mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stopsSortedBySequenceNumber
                   in find
                        ( \stop ->
                            maybe
                              False
                              (\startStopSequence -> stop.stopCode == endStationCode && stop.sequenceNum > startStopSequence)
                              mbStartStopSequence
                        )
                        stopsSortedBySequenceNumber
                        <&> ( \endStop -> do
                                case mbStartStopSequence of
                                  Just startStopSequence ->
                                    let intermediateStops = filter (\stop -> stop.sequenceNum >= startStopSequence && stop.sequenceNum <= endStop.sequenceNum) stopsSortedBySequenceNumber
                                        totalStops = endStop.sequenceNum - startStopSequence
                                        totalTravelTime =
                                          foldr
                                            ( \stop acc ->
                                                if stop.sequenceNum > startStopSequence && stop.sequenceNum <= endStop.sequenceNum
                                                  then case (acc, stop.estimatedTravelTimeFromPreviousStop) of
                                                    (Just acc', Just travelTime) -> Just (acc' + travelTime)
                                                    _ -> Nothing
                                                  else acc
                                            )
                                            (Just $ Seconds 0)
                                            stops
                                     in (endStop.routeCode, Just totalStops, totalTravelTime, Just intermediateStops)
                                  Nothing -> (endStop.routeCode, Nothing, Nothing, Nothing)
                            )
              )
              groupedStops
  let mappedRouteCodes = map (\(routeCode, _, _, _) -> routeCode) possibleRoutes
  routes <- mapM (\routeCode -> OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound $ "RouteCode:" +|| routeCode ||+ "and integratedBPPConfigId: " +|| integratedBPPConfig.id.getId ||+ "")) mappedRouteCodes

  return $
    map
      ( \route ->
          let routeData = find (\(routeCode, _, _, _) -> routeCode == route.code) possibleRoutes
           in RouteStopInfo
                { route,
                  totalStops = (\(_, totalStops, _, _) -> totalStops) =<< routeData,
                  stops = (\(_, _, _, stops) -> stops) =<< routeData,
                  startStopCode = startStationCode,
                  endStopCode = endStationCode,
                  travelTime = (\(_, _, travelTime, _) -> travelTime) =<< routeData
                }
      )
      routes

getPossibleRoutesBetweenTwoParentStops :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => Text -> Text -> IntegratedBPPConfig -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoParentStops startParentStopCode endParentStopCode integratedBPPConfig = do
  -- Get all child station codes for both parent stops
  startStops <- OTPRest.getChildrenStationsCodes integratedBPPConfig startParentStopCode
  endStops <- OTPRest.getChildrenStationsCodes integratedBPPConfig endParentStopCode

  -- If no children found, use the parent stop codes themselves as fallback
  let actualStartStops = if null startStops then [startParentStopCode] else startStops
      actualEndStops = if null endStops then [endParentStopCode] else endStops
      allStopCodes = nub (actualStartStops ++ actualEndStops)

  routesWithStops <- OTPRest.getRouteStopMappingByStopCodes integratedBPPConfig allStopCodes
  let routeCodes = nub $ map (.routeCode) routesWithStops

  -- Get all route stop mappings for these routes in one go
  allRouteStops <- concatMapM (\routeCode -> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig) routeCodes

  -- Filter routes based on time bounds
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain allRouteStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) allRouteStops
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) $ sortBy (compare `on` (.routeCode)) serviceableStops

      -- Find routes that connect any start stop to any end stop
      possibleRoutes = nub $ catMaybes $ map (findValidRouteForParentStops actualStartStops actualEndStops) groupedStops

  -- Build route info for valid routes
  let mappedRouteCodes = map (\(routeCode, _, _, _, _, _) -> routeCode) possibleRoutes
  routes <- mapM (\routeCode -> OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound $ "RouteCode:" +|| routeCode ||+ "and integratedBPPConfigId: " +|| integratedBPPConfig.id.getId ||+ "")) mappedRouteCodes

  return $
    map
      ( \route ->
          let routeData = find (\(routeCode, _, _, _, _, _) -> routeCode == route.code) possibleRoutes
           in RouteStopInfo
                { route,
                  totalStops = (\(_, totalStops, _, _, _, _) -> totalStops) =<< routeData,
                  stops = (\(_, _, _, stops, _, _) -> stops) =<< routeData,
                  startStopCode = fromMaybe startParentStopCode ((\(_, _, _, _, startStop, _) -> startStop) <$> routeData),
                  endStopCode = fromMaybe endParentStopCode ((\(_, _, _, _, _, endStop) -> endStop) <$> routeData),
                  travelTime = (\(_, _, travelTime, _, _, _) -> travelTime) =<< routeData
                }
      )
      routes
  where
    -- Helper function to find valid routes between parent stops
    findValidRouteForParentStops :: [Text] -> [Text] -> [RouteStopMapping.RouteStopMapping] -> Maybe (Text, Maybe Int, Maybe Seconds, Maybe [RouteStopMapping.RouteStopMapping], Text, Text)
    findValidRouteForParentStops startStopCodes endStopCodes stops =
      let stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) stops
          -- Find all possible start stops in this route
          startStopsInRoute = filter (\stop -> stop.stopCode `elem` startStopCodes) stopsSortedBySequenceNumber
          -- Find all possible end stops in this route
          endStopsInRoute = filter (\stop -> stop.stopCode `elem` endStopCodes) stopsSortedBySequenceNumber

          -- Find the best start-end combination
          bestCombination = do
            startStop <- listToMaybe startStopsInRoute -- Get earliest start stop
            endStop <- find (\endStop -> endStop.sequenceNum > startStop.sequenceNum) endStopsInRoute -- Get first valid end stop
            return (startStop, endStop)
       in case bestCombination of
            Just (startStop, endStop) ->
              let intermediateStops = filter (\stop -> stop.sequenceNum >= startStop.sequenceNum && stop.sequenceNum <= endStop.sequenceNum) stopsSortedBySequenceNumber
                  totalStops = endStop.sequenceNum - startStop.sequenceNum
                  totalTravelTime =
                    foldr
                      ( \stop acc ->
                          if stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum <= endStop.sequenceNum
                            then case (acc, stop.estimatedTravelTimeFromPreviousStop) of
                              (Just acc', Just travelTime) -> Just (acc' + travelTime)
                              _ -> Nothing
                            else acc
                      )
                      (Just $ Seconds 0)
                      stops
               in Just (startStop.routeCode, Just totalStops, totalTravelTime, Just intermediateStops, startStop.stopCode, endStop.stopCode)
            Nothing -> Nothing

data FRFSTicketCategory = FRFSTicketCategory
  { category :: FRFSQuoteCategoryType,
    price :: Price,
    offeredPrice :: Price,
    bppItemId :: Text,
    eligibility :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FRFSVehicleServiceTier = FRFSVehicleServiceTier
  { serviceTierType :: Spec.ServiceTierType,
    serviceTierProviderCode :: Text,
    serviceTierShortName :: Text,
    serviceTierDescription :: Text,
    serviceTierLongName :: Text,
    isAirConditioned :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FRFSFare = FRFSFare
  { farePolicyId :: Maybe (Id DFRFSFarePolicy.FRFSFarePolicy),
    categories :: [FRFSTicketCategory],
    fareDetails :: Maybe Quote.FRFSFareDetails,
    vehicleServiceTier :: FRFSVehicleServiceTier,
    fareQuoteType :: Maybe Quote.FRFSQuoteType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

getFare :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Id IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFare riderId vehicleType serviceTier integratedBPPConfigId merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  now <- getCurrentTime
  fareProducts <- case serviceTier of
    Just serviceTier' -> do
      vehicleServiceTier <- QFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier' merchantOperatingCityId integratedBPPConfigId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> show serviceTier')
      maybeToList <$> QFRFSRouteFareProduct.findByRouteCodeAndVehicleServiceTierId routeCode vehicleServiceTier.id
    Nothing -> QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfigId
  let serviceableFareProducts = DTB.findBoundedDomain fareProducts now ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
  logDebug $ "Serviceable Fare Products Debug: " <> show serviceableFareProducts
  logDebug $ "Route Code Debug: " <> routeCode <> " Debug Service Tier: " <> show serviceTier
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigById integratedBPPConfigId
  mapM (buildFRFSFare riderId vehicleType merchantId merchantOperatingCityId routeCode startStopCode endStopCode integratedBPPConfig) serviceableFareProducts

buildFRFSFare :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Spec.VehicleCategory -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> IntegratedBPPConfig -> FRFSRouteFareProduct -> m FRFSFare
buildFRFSFare _riderId _vehicleType _merchantId _merchantOperatingCityId routeCode startStopCode endStopCode integratedBPPConfig fareProduct = do
  vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
  farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
  let cessCharge = fromMaybe (HighPrecMoney 0) farePolicy.cessCharge
  categories <-
    case farePolicy._type of
      DFRFSFarePolicy.MatrixBased -> do
        routeStopFares <- QRouteStopFare.findAllByStartStopAndIntegratedBPPConfigId startStopCode endStopCode integratedBPPConfig.id
        case routeStopFares of
          [] -> throwError $ InternalError "FRFS Route Stop Fare Not Found"
          fares -> do
            let faresForPolicy = filter (\stopFare -> stopFare.farePolicyId == farePolicy.id) fares
            return $
              map
                ( \stopFare ->
                    FRFSTicketCategory
                      { category = stopFare.category,
                        price =
                          Price
                            { amountInt = roundToIntegral stopFare.amount,
                              amount = stopFare.amount,
                              currency = stopFare.currency
                            },
                        offeredPrice =
                          Price
                            { amountInt = roundToIntegral $ fromMaybe stopFare.amount stopFare.offeredAmount,
                              amount = fromMaybe stopFare.amount stopFare.offeredAmount,
                              currency = stopFare.currency
                            },
                        bppItemId = fromMaybe (getProviderName integratedBPPConfig) stopFare.bppItemId,
                        eligibility = True
                      }
                )
                faresForPolicy
      DFRFSFarePolicy.StageBased -> do
        stageFares <- QFRFSStageFare.findAllByFarePolicyId farePolicy.id
        startStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeCode startStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
        endStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeCode endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
        let stage = max 1 (abs $ endStageFare.stage - startStageFare.stage) -- if stage is 0, then it is the same stage so we take 1 as the stage
        stageFare <- find (\stageFare -> stageFare.stage == stage) stageFares & fromMaybeM (InternalError "FRFS Stage Fare Not Found")
        let amount = stageFare.amount + cessCharge
        let price =
              Price
                { amountInt = roundToIntegral amount,
                  amount = amount,
                  currency = stageFare.currency
                }
        -- For StageBased, create a single ADULT category
        return
          [ FRFSTicketCategory
              { category = ADULT,
                price = price,
                offeredPrice = price,
                bppItemId = getProviderName integratedBPPConfig,
                eligibility = True
              }
          ]
  return $
    FRFSFare
      { farePolicyId = Just farePolicy.id,
        categories = categories,
        fareDetails = Nothing,
        vehicleServiceTier =
          FRFSVehicleServiceTier
            { serviceTierType = vehicleServiceTier._type,
              serviceTierProviderCode = vehicleServiceTier.providerCode,
              serviceTierShortName = vehicleServiceTier.shortName,
              serviceTierDescription = vehicleServiceTier.description,
              serviceTierLongName = vehicleServiceTier.longName,
              isAirConditioned = vehicleServiceTier.isAirConditioned
            },
        fareQuoteType = Nothing
      }

getFareThroughGTFS :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DP.Person -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFareThroughGTFS _riderId vehicleType serviceTier integratedBPPConfig _merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  tripDetails <- OTPRest.getExampleTrip integratedBPPConfig routeCode
  case tripDetails of
    Just trip -> do
      let startStop = OTPRest.findTripStopByStopCode trip startStopCode
          endStop = OTPRest.findTripStopByStopCode trip endStopCode
      logDebug $ "startStop: " <> show startStop <> " endStop: " <> show endStop
      case (startStop, endStop) of
        (Just startTripStop, Just endTripStop) -> do
          let startStage = OTPRest.extractStageFromTripStop startTripStop
              endStage = OTPRest.extractStageFromTripStop endTripStop
              startIsStageStop = OTPRest.extractIsStageStopFromTripStop startTripStop
              endIsStageStop = OTPRest.extractIsStageStopFromTripStop endTripStop
          case (startStage, endStage) of
            (Just startStageNum, Just endStageNum) -> do
              let stage = abs (endStageNum - startStageNum)
              logDebug $ "isStageStop flags: startStop=" <> show startIsStageStop <> " endStop=" <> show endIsStageStop
              let adjustedStage = case endIsStageStop of
                    Just True -> stage - 1 -- Reduce stage by 1 if found, but ensure minimum is 1
                    _ -> stage -- Use original stage if not found or Nothing
              fares <- case serviceTier of
                Just serviceTier' -> do
                  vehicleServiceTier <- QFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTier' merchantOperatingCityId integratedBPPConfig.id >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> show serviceTier')
                  maybeToList <$> QQFRFSGtfsStageFare.findOneByVehicleTypeAndStageAndMerchantOperatingCityIdAndVehicleServiceTierId vehicleType (max 0 adjustedStage) merchantOperatingCityId vehicleServiceTier.id
                Nothing -> QFRFSGtfsStageFare.findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType (max 0 adjustedStage) merchantOperatingCityId
              forM fares $ \fare -> do
                vehicleServiceTier <- QFRFSVehicleServiceTier.findById fare.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fare.vehicleServiceTierId.getId)
                let price = Price {amountInt = roundToIntegral (fare.amount + fromMaybe 0 fare.cessCharge), amount = fare.amount + fromMaybe 0 fare.cessCharge, currency = fare.currency}
                return $
                  FRFSFare
                    { farePolicyId = Nothing,
                      categories =
                        [ FRFSTicketCategory
                            { category = ADULT,
                              price = price,
                              offeredPrice = price,
                              bppItemId = getProviderName integratedBPPConfig,
                              eligibility = True
                            }
                        ],
                      fareDetails = Nothing,
                      vehicleServiceTier =
                        FRFSVehicleServiceTier
                          { serviceTierType = vehicleServiceTier._type,
                            serviceTierProviderCode = vehicleServiceTier.providerCode,
                            serviceTierShortName = vehicleServiceTier.shortName,
                            serviceTierDescription = vehicleServiceTier.description,
                            serviceTierLongName = vehicleServiceTier.longName,
                            isAirConditioned = vehicleServiceTier.isAirConditioned
                          },
                      fareQuoteType = Nothing
                    }
            _ -> return [] -- No stage information available
        _ -> return [] -- Start or end stop not found in trip
    Nothing -> return [] -- Trip details not found

getFares :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DP.Person -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFares riderId vehicleType serviceTier integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  faresResult <- withTryCatch "getFareThroughGTFS:getFares" (getFareThroughGTFS riderId vehicleType serviceTier integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode)
  fares <- case faresResult of
    Left err -> do
      logError $ "Error in getFareThroughGTFS (GraphQL/GTFS): " <> show err
      return []
    Right fares' -> return fares'

  if null fares
    then do
      withTryCatch "getFare:getFares" (getFare riderId vehicleType serviceTier integratedBPPConfig.id merchantId merchantOperatingCityId routeCode startStopCode endStopCode)
        >>= \case
          Left err -> do
            logError $ "Error in getFare: " <> show err
            return []
          Right fares' -> return fares'
    else return fares

data VehicleTracking = VehicleTracking
  { nextStop :: Maybe RouteStopMapping.RouteStopMapping,
    nextStopTravelTime :: Maybe Seconds,
    nextStopTravelDistance :: Maybe Meters,
    upcomingStops :: [UpcomingStop],
    vehicleId :: Text,
    vehicleInfo :: Maybe VehicleInfo,
    delay :: Maybe Seconds,
    routeShortName :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpcomingStop = UpcomingStop
  { stopName :: Text,
    stopCode :: Text,
    stopSeq :: Int,
    travelDistance :: Maybe Meters,
    estimatedTravelTime :: Maybe UTCTime,
    actualTravelTime :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo
  { latitude :: Maybe Double,
    longitude :: Maybe Double,
    scheduleRelationship :: Maybe Text,
    routeState :: Maybe CQMMB.RouteState,
    speed :: Maybe Double,
    startDate :: Maybe Text,
    startTime :: Maybe UTCTime,
    timestamp :: Maybe Text,
    tripId :: Maybe Text,
    upcomingStops :: Maybe [LT.UpcomingStop]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

trackVehicles :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv), HasField "cloudType" r (Maybe CloudType), HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Maybe LatLong -> Maybe (Id DIBC.IntegratedBPPConfig) -> m [VehicleTracking]
trackVehicles _personId _merchantId merchantOpCityId vehicleType routeCode platformType mbRiderPosition mbIntegratedBPPConfigId = do
  now <- getCurrentTime
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOpCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType
  case vehicleType of
    Spec.BUS -> do
      case platformType of
        DIBC.APPLICATION -> do
          vehicleTrackingInfo <- getVehicleTrackingInfo integratedBPPConfig
          mapM
            ( \(vehicleId, vehicleInfo) -> do
                upcomingStop <-
                  case vehicleInfo.upcomingStops of
                    Just upcomingStops -> do
                      let mbUpcomingStop = find (\upcomingStop -> upcomingStop.status == LT.Upcoming) upcomingStops
                      case mbUpcomingStop of
                        Just upcomingStop' -> do
                          upcomingStopNew <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode upcomingStop'.stop.stopCode routeCode integratedBPPConfig
                          return $ listToMaybe upcomingStopNew
                        Nothing -> return Nothing
                    Nothing -> return Nothing
                pure $
                  VehicleTracking
                    { nextStop = upcomingStop,
                      nextStopTravelTime = Nothing,
                      nextStopTravelDistance = Nothing,
                      upcomingStops = [],
                      vehicleId = vehicleId,
                      vehicleInfo = Just vehicleInfo,
                      delay = Nothing,
                      routeShortName = Nothing
                    }
            )
            vehicleTrackingInfo
        _ -> do
          nearbyBuses <- CQMMB.getRoutesBuses routeCode integratedBPPConfig -- Add a new logic to get the bus location and ETA, unify it with the existing logic @khuzema
          routeStopMapping <- HM.fromList . map (\a -> (a.stopCode, a)) <$> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
          nearbyBuses.buses `forM` \bus -> do
            let busData = bus.busData
            let mbNextStop = busData.eta_data >>= listToMaybe
            let mbNextStopMapping = mbNextStop >>= (\stop -> HM.lookup stop.stopCode routeStopMapping)
            let (_, upcomingStops) =
                  foldr'
                    ( \stop (lastPoint, acc) -> do
                        let mbStop = HM.lookup stop.stopCode routeStopMapping
                        case mbStop of
                          Just stop' -> do
                            let us =
                                  UpcomingStop
                                    { stopCode = stop.stopCode,
                                      stopSeq = stop'.sequenceNum,
                                      stopName = stop'.stopName,
                                      estimatedTravelTime = Just stop.arrivalTime,
                                      travelDistance = fmap highPrecMetersToMeters (\lastPoint' -> distanceBetweenInMeters lastPoint' (mkLatLong stop'.stopPoint.lat stop'.stopPoint.lon)) <$> lastPoint,
                                      actualTravelTime = Nothing
                                    }
                            (Just (mkLatLong stop'.stopPoint.lat stop'.stopPoint.lon), us : acc)
                          Nothing -> (lastPoint, acc)
                    )
                    (mbRiderPosition, [])
                    (fromMaybe [] busData.eta_data)
            logDebug $ "Got bus data for route " <> routeCode <> ": next stop" <> show mbNextStopMapping
            return $
              VehicleTracking
                { nextStop = mbNextStopMapping,
                  nextStopTravelTime = (\t -> Seconds $ getSeconds (nominalDiffTimeToSeconds $ diffUTCTime t (utcToIST now)) `div` 60) <$> (mbNextStop <&> (.arrivalTime)),
                  nextStopTravelDistance = Nothing,
                  upcomingStops = upcomingStops, -- fix it later
                  vehicleId = bus.vehicleNumber,
                  routeShortName = busData.route_number,
                  vehicleInfo =
                    Just $
                      VehicleInfo
                        { latitude = Just busData.latitude,
                          longitude = Just busData.longitude,
                          scheduleRelationship = Nothing,
                          speed = Nothing,
                          startDate = Nothing,
                          startTime = Nothing,
                          routeState = busData.route_state,
                          timestamp = Just . show $ epochToUTCTime busData.timestamp,
                          tripId = Nothing,
                          upcomingStops = Nothing
                        },
                  delay = Nothing
                }
    _ -> do
      route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
      routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig

      let waypointsForRoute' = case route.polyline of
            Just polyline -> Just $ KEPP.decode polyline
            Nothing -> Nothing

      case waypointsForRoute' of
        Just waypointsForRoute -> do
          let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) routeStops
              stopPairs = pairWithNext sortedStops
          stopPairsWithWaypoints <- getStopPairsWithWaypointsForMetroAndSubway stopPairs waypointsForRoute
          let riderPosition = maybe [] (\latLong -> [(latLong.lat, latLong.lon)]) mbRiderPosition
          forM riderPosition $ \(vehicleLat, vehicleLon) -> do
            minDistancesWithWaypoints <-
              forM stopPairsWithWaypoints $ \((_currStop, nextStop), (waypoints, _duration)) -> do
                let (groupedWaypoints, _) =
                      foldr
                        ( \point (distanceFromVehicleAndSubsequentWaypoints, subsequentWaypointsIncludingCurrentPoint) ->
                            let distanceFromVehicle = highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLong vehicleLat vehicleLon) point
                                subsequentWaypointsExcludingCurrentPoint = tail subsequentWaypointsIncludingCurrentPoint
                             in (distanceFromVehicleAndSubsequentWaypoints <> [(distanceFromVehicle, subsequentWaypointsIncludingCurrentPoint)], subsequentWaypointsExcludingCurrentPoint)
                        )
                        ([], waypoints)
                        waypoints
                let minDistanceFromVehicle = minimumBy (comparing fst) groupedWaypoints
                pure (minDistanceFromVehicle, nextStop)
            let ((_, _), nextStop) = minimumBy (comparing fst) minDistancesWithWaypoints

            logDebug $ "Next stop: " <> show nextStop
            let vehicleTracking =
                  VehicleTracking
                    { nextStop = Just nextStop,
                      nextStopTravelTime = Nothing,
                      nextStopTravelDistance = Nothing,
                      upcomingStops = [],
                      routeShortName = Nothing,
                      vehicleId = show vehicleType,
                      vehicleInfo = Nothing,
                      delay = Nothing
                    }
            pure vehicleTracking
        Nothing -> do
          logDebug $ "Waypoints for route not found."
          pure []
  where
    getStopPairsWithWaypointsForMetroAndSubway stopPairs waypoints =
      forM stopPairs $ \(currStop, nextStop) -> do
        let waypointsBetweenStops = fromMaybe [] (getWaypointsBetweenStops currStop.stopPoint nextStop.stopPoint waypoints)
        pure ((currStop, nextStop), (waypointsBetweenStops, Nothing :: Maybe Seconds))

    epochToUTCTime epoch = posixSecondsToUTCTime (fromIntegral epoch)

    getWaypointsBetweenStops curStopPoint nextStopPoint waypoints = do
      let nearestToCurStop = findNearestWaypoint curStopPoint waypoints
      let nearestToNextStop = findNearestWaypoint nextStopPoint waypoints
      case (nearestToCurStop, nearestToNextStop) of
        (Just wpA, Just wpB) ->
          Just $ takeUntil wpB $ dropWhile (/= wpA) waypoints
        _ -> Just []
    findNearestWaypoint point waypoints =
      listToMaybe $ sortBy (comparing $ distanceBetweenInMeters point) waypoints

    takeUntil y = foldr (\x acc -> x : if x == y then [] else acc) []

    getVehicleTrackingInfo integratedBPPConfig = do
      vehicleInfoByRouteCode :: [(Text, VehicleInfo)] <- do
        vehicleTrackingResp <- LF.vehicleTrackingOnRoute (LF.ByRoute routeCode)
        pure $ mkVehicleInfo vehicleTrackingResp
      if null vehicleInfoByRouteCode
        then do
          tripIds <- map DRTM.tripCode <$> QRouteTripMapping.findAllTripIdByRouteCode routeCode integratedBPPConfig.id
          vehicleTrackingResp <- LF.vehicleTrackingOnRoute (LF.ByTrips tripIds)
          pure $ mkVehicleInfo vehicleTrackingResp
        else pure vehicleInfoByRouteCode

    mkVehicleInfo :: [LT.VehicleTrackingOnRouteResp] -> [(Text, VehicleInfo)]
    mkVehicleInfo vehiclesInfo =
      vehiclesInfo
        <&> ( \vehicleInfo ->
                ( vehicleInfo.vehicleNumber,
                  VehicleInfo
                    { latitude = Just vehicleInfo.vehicleInfo.latitude,
                      longitude = Just vehicleInfo.vehicleInfo.longitude,
                      scheduleRelationship = vehicleInfo.vehicleInfo.scheduleRelationship,
                      speed = vehicleInfo.vehicleInfo.speed,
                      startDate =
                        ( \startTime ->
                            T.pack $
                              Time.formatTime
                                Time.defaultTimeLocale
                                "%d-%m-%Y"
                                ( addUTCTime
                                    (secondsToNominalDiffTime 19800)
                                    startTime
                                )
                        )
                          <$> vehicleInfo.vehicleInfo.startTime,
                      startTime = vehicleInfo.vehicleInfo.startTime,
                      timestamp = vehicleInfo.vehicleInfo.timestamp,
                      tripId = vehicleInfo.vehicleInfo.tripId,
                      upcomingStops = vehicleInfo.vehicleInfo.upcomingStops,
                      routeState = Nothing
                    }
                )
            )

    mkLatLong :: Double -> Double -> Maps.LatLong
    mkLatLong lat_ lon_ =
      Maps.LatLong
        { lat = lat_,
          lon = lon_
        }

    pairWithNext :: [a] -> [(a, a)]
    pairWithNext xs = zip xs (tail xs)

getDiscountInfo :: Bool -> Maybe Int -> Maybe Int -> Price -> Int -> Int -> (Maybe Int, Maybe HighPrecMoney)
getDiscountInfo isEventOngoing mbFreeTicketInterval mbMaxFreeTicketCashback price quantity ticketsBookedInEvent =
  let freeTicketInterval = fromMaybe (maxBound :: Int) mbFreeTicketInterval
      maxFreeTicketCashback = fromMaybe 0 mbMaxFreeTicketCashback
   in if isEventOngoing
        then
          let perTicketCashback = min maxFreeTicketCashback price.amountInt.getMoney
              discountedTickets = ((ticketsBookedInEvent + quantity) `div` freeTicketInterval) - (ticketsBookedInEvent `div` freeTicketInterval)
              eventDiscountAmount = toHighPrecMoney $ discountedTickets * perTicketCashback
           in (Just discountedTickets, Just eventDiscountAmount)
        else (Nothing, Nothing)

partnerOrgRiderId :: Id DP.Person
partnerOrgRiderId = Id "partnerOrg_rider_id"

partnerOrgBppItemId :: Text
partnerOrgBppItemId = "partnerOrg_bpp_item_id"

partnerOrgBppSubscriberId :: Text
partnerOrgBppSubscriberId = "partnerOrg_bpp_subscriber_id"

partnerOrgBppSubscriberUrl :: Text
partnerOrgBppSubscriberUrl = "partnerOrg_bpp_subscriber_url"

getJourneyIdFromBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (Maybe (Id DJourney.Journey))
getJourneyIdFromBooking booking = do
  mbJourneyLeg <- QJL.findByLegSearchId (Just booking.searchId.getId)
  return $ mbJourneyLeg <&> (.journeyId)

getAllJourneyFrfsBookings ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (Maybe (Id DJourney.Journey), [DFRFSTicketBooking.FRFSTicketBooking])
getAllJourneyFrfsBookings booking = do
  mbJourneyLeg <- QJL.findByLegSearchId (Just booking.searchId.getId)
  case mbJourneyLeg of
    Just leg -> do
      legs <- QJL.getJourneyLegs leg.journeyId
      bookings <- mapMaybeM (QFRFSTicketBooking.findBySearchId . Id) (mapMaybe (.legSearchId) legs)
      return (Just leg.journeyId, bookings)
    Nothing -> pure (Nothing, [booking])

getQuoteOfferSegment ::
  (EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  Maybe Text ->
  m (Maybe Text)
getQuoteOfferSegment riderId merchantOperatingCityId vehicleType mbRouteStationsJson = do
  result <- withTryCatch "getQuoteOfferSegment" $ do
    person <- QPerson.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
    SOfferSegment.getPersonOfferSegment person merchantOperatingCityId $
      SOfferSegment.ticketContext (Just vehicleType) (getServiceTierTypeFromRouteStationsJson mbRouteStationsJson)
  case result of
    Right segment -> pure segment
    Left err -> do
      logError $ "getQuoteOfferSegment failed for rider " <> riderId.getId <> ", quote left unsegmented: " <> show err
      pure Nothing

getOfferSegmentUdf2 ::
  (BeamFlow m r) =>
  Bool ->
  Maybe (Id Quote.FRFSQuote) ->
  m (Maybe Text)
getOfferSegmentUdf2 True (Just quoteId) = do
  mbQuote <- QFRFSQuote.findById quoteId
  pure $ mbQuote >>= (.offerSegment)
getOfferSegmentUdf2 _ _ = pure Nothing

-- | Which grouping mechanism, if any, ties this booking's payment to sibling bookings'.
-- A cart's bookingGroupId always takes priority over the trivial per-booking Journey every
-- FRFS booking still gets wrapped in (see buildJourneyAndLeg) -- that Journey exists for
-- history/live-tracking, not for payment grouping, once a bookingGroupId is present.
data BookingPaymentGroup
  = JourneyPaymentGroup (Id DJourney.Journey)
  | BookingGroupPaymentGroup (Id DFRFSBookingGroup.FRFSBookingGroup)
  | NoPaymentGroup

getBookingPaymentSiblings ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (BookingPaymentGroup, [DFRFSTicketBooking.FRFSTicketBooking])
getBookingPaymentSiblings booking = case booking.bookingGroupId of
  Just bookingGroupId -> do
    bookings <- QFRFSTicketBooking.findAllByBookingGroupId (Just bookingGroupId)
    return (BookingGroupPaymentGroup bookingGroupId, bookings)
  Nothing -> do
    (mbJourneyId, bookings) <- getAllJourneyFrfsBookings booking
    return (maybe NoPaymentGroup JourneyPaymentGroup mbJourneyId, bookings)

createPaymentOrder ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    Finance.HasActorInfo m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id DMOC.MerchantOperatingCity ->
  Id Merchant.Merchant ->
  HighPrecMoney ->
  DP.Person ->
  Payment.PaymentServiceType ->
  [Payment.VendorSplitDetails] ->
  [Payment.Basket] ->
  Bool ->
  m (Maybe DOrder.PaymentOrder)
createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket isMockPayment
  -- A fully pass-covered set of bookings has nothing to charge, so there is no order to create.
  -- Returning Nothing here is what lets the confirm flow skip payment entirely rather than
  -- creating a zero-amount order the gateway would reject.
  | amount <= 0 && not (null bookings) && all (FRFSPassOverride.isFullyPassCovered . (.overriddenAmount)) bookings = do
    logInfo $ "createPaymentOrder: skipping order, fully overridden bookings=" <> show (bookings <&> (.id.getId))
    pure Nothing
  | otherwise = do
    nwAddress <- asks (.nwAddress)
    logInfo $ "createPayments vendorSplitArr" <> show vendorSplitArr
    logInfo $ "createPayments basket" <> show basket
    personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
    personEmail <- mapM decrypt person.email
    (orderId, orderShortId) <- getPaymentIds
    results <- processPayments orderId `mapM` bookings
    let (ticketBookingPayments', allPaymentCategories) = unzip results
    QFRFSTicketBookingPaymentCategory.createMany (concat allPaymentCategories)
    QFRFSTicketBookingPayment.createMany ticketBookingPayments'
    isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentType
    isPercentageSplitEnabled <- Payment.getIsPercentageSplit merchantId merchantOperatingCityId Nothing paymentType
    let isSingleMode = case bookings of
          [_] -> True
          _ -> False
    splitSettlementDetails <- Payment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amount vendorSplitArr isPercentageSplitEnabled isSingleMode
    staticCustomerId <- SLUtils.getStaticCustomerId person personPhone
    udf1 <- SLUtils.getPersonUdf1 person
    udf2 <- getOfferSegmentUdf2 isSingleMode ((.quoteId) <$> listToMaybe bookings)
    let createOrderReq =
          Payment.CreateOrderReq
            { orderId = orderId.getId,
              orderShortId = orderShortId,
              amount = amount,
              customerId = staticCustomerId,
              customerEmail = fromMaybe "growth@nammayatri.in" personEmail,
              customerPhone = personPhone,
              customerFirstName = person.firstName,
              customerLastName = person.lastName,
              createMandate = Nothing,
              mandateMaxAmount = Nothing,
              mandateFrequency = Nothing,
              mandateEndDate = Nothing,
              mandateStartDate = Nothing,
              optionsGetUpiDeepLinks = Nothing,
              metadataExpiryInMins = Nothing,
              metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
              webhookUrl = Just nwAddress,
              splitSettlementDetails = splitSettlementDetails,
              basket = basket,
              paymentRules = Nothing,
              autoRefundPostSuccess = Nothing,
              paymentFilter = Nothing,
              udf1 = udf1,
              udf2 = udf2
            }
    let mocId = merchantOperatingCityId
        commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId
        commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
        commonMerchantOperatingCityId = Kernel.Types.Id.cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
        createOrderCall = Payment.createOrder merchantId mocId Nothing paymentType (Just person.id.getId) person.clientSdkVersion (Just isMockPayment)
    mbPaymentOrderValidTill <- Payment.getPaymentOrderValidity merchantId merchantOperatingCityId Nothing paymentType
    isMetroTestTransaction <- asks (.isMetroTestTransaction)
    let createWalletCall = TWallet.createWallet merchantId merchantOperatingCityId
        groupId = listToMaybe $ sort (bookings <&> (.id.getId))
    orderResp <- DPayment.createOrderService commonMerchantId (Just $ cast mocId) commonPersonId mbPaymentOrderValidTill Nothing paymentType isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) isMockPayment groupId
    mapM (\resp -> DPayment.buildPaymentOrder commonMerchantId (Just commonMerchantOperatingCityId) commonPersonId mbPaymentOrderValidTill Nothing paymentType createOrderReq resp isMockPayment groupId Nothing) orderResp
  where
    getPaymentIds = do
      orderShortId <- generateShortId
      orderId <- generateGUID
      return (orderId, orderShortId.getShortId)

    processPayments ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      Id PaymentOrder.PaymentOrder ->
      FTBooking.FRFSTicketBooking ->
      m (DFRFSTicketBookingPayment.FRFSTicketBookingPayment, [DTBPC.FRFSTicketBookingPaymentCategory])
    processPayments orderId booking = do
      ticketBookingPaymentId <- generateGUID
      now <- getCurrentTime
      let ticketBookingPayment =
            DFRFSTicketBookingPayment.FRFSTicketBookingPayment
              { frfsTicketBookingId = booking.id,
                frfsQuoteId = Just booking.quoteId,
                id = ticketBookingPaymentId,
                status = DFRFSTicketBookingPayment.PENDING,
                merchantId = Just booking.merchantId,
                merchantOperatingCityId = Just booking.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now,
                paymentOrderId = orderId
              }
      -- Fetch quote categories and create payment categories
      quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
      paymentCategories <- mapM (mkPaymentCategory ticketBookingPaymentId booking now) quoteCategories
      return (ticketBookingPayment, paymentCategories)

    mkPaymentCategory ::
      (MonadFlow m) =>
      Id DFRFSTicketBookingPayment.FRFSTicketBookingPayment ->
      FTBooking.FRFSTicketBooking ->
      UTCTime ->
      DFRFSQuoteCategory.FRFSQuoteCategory ->
      m DTBPC.FRFSTicketBookingPaymentCategory
    mkPaymentCategory paymentId booking now quoteCategory = do
      categoryId <- generateGUID
      return $
        DTBPC.FRFSTicketBookingPaymentCategory
          { id = categoryId,
            frfsTicketBookingPaymentId = paymentId,
            quoteId = quoteCategory.quoteId,
            bppItemId = quoteCategory.bppItemId,
            category = quoteCategory.category,
            categoryMeta = quoteCategory.categoryMeta,
            price = quoteCategory.price,
            offeredPrice = quoteCategory.offeredPrice,
            finalPrice = quoteCategory.finalPrice,
            selectedQuantity = quoteCategory.selectedQuantity,
            merchantId = booking.merchantId,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            seatIds = quoteCategory.seatIds,
            seatLabels = quoteCategory.seatLabels,
            holdId = quoteCategory.holdId
          }

makecancelledTtlKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
makecancelledTtlKey bookingId = "FRFS:OnConfirm:CancelledTTL:bookingId-" <> bookingId.getId

-- | A rider's cancellation allowance in one operating city.
-- Design notes: scripts/testing/cancel/DESIGN.md
data CancellationQuota = CancellationQuota
  { quotaKey :: Text,
    quotaMember :: Text,
    maxCancellations :: Int,
    windowSeconds :: Int,
    source :: QuotaSource
  }

-- | Take a booking from NEW to CONFIRMING atomically, and return it as it was when claimed.
--
-- Nothing means someone else already claimed it and the caller must not confirm. Every path that
-- confirms with the BPP goes through this: the transition is a read then a write, so unguarded, two
-- callers -- concurrent confirm requests for one searchId, or a retried payment webhook racing a
-- status poll -- both see NEW, both call the BPP, and one booking gets two tickets and two debits
-- at on_confirm.
--
-- validTill is written inside the lock and BEFORE the status flip. Until it lands the booking is
-- CONFIRMING while still carrying the validTill it had as NEW, and frfsBookingStatus fails a
-- CONFIRMING booking whose validTill has passed -- so the other order lets a status poll kill a
-- leg that is about to be confirmed.
--
-- The lock covers ONLY the claim. Holding it across the BPP call would be worse than not locking:
-- waiters spin on a microsecond retry delay, and a call slower than the lock TTL would let a second
-- caller in while the first is still working, after which the first one's release deletes the
-- second one's lock.
claimBookingForConfirm ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  UTCTime ->
  m (Maybe DFRFSTicketBooking.FRFSTicketBooking)
claimBookingForConfirm bookingId validTill =
  Redis.withWaitAndLockRedis (confirmClaimLockKey bookingId) confirmClaimLockTtlSec confirmClaimLockRetryDelayMicros $ do
    latest <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest $ "Invalid booking id " <> bookingId.getId)
    if latest.status /= DFRFSTicketBooking.NEW
      then pure Nothing
      else do
        void $ QFRFSTicketBooking.updateValidTillById validTill latest.id
        void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.CONFIRMING latest.id
        -- Carry the validTill we just wrote, not the stale one: ACL.buildConfirmReq falls back to
        -- booking.validTill for the confirm TTL when bapConfig.confirmTTLSec is unset, so returning
        -- the pre-update record would send the operator an expiry we have already replaced.
        pure (Just latest {DFRFSTicketBooking.validTill = validTill})

confirmClaimLockKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
confirmClaimLockKey bookingId = "FRFSConfirm:claimBooking-" <> bookingId.getId

-- The critical section is one read and two writes, so this only has to outlive a few KV round
-- trips. Short on purpose: a TTL that can expire mid-section stops being a lock.
confirmClaimLockTtlSec :: Int
confirmClaimLockTtlSec = 10

-- Waiters retry on this delay. A contended claim resolves in milliseconds, so the loser sleeps once
-- and finds the booking already CONFIRMING.
confirmClaimLockRetryDelayMicros :: Int
confirmClaimLockRetryDelayMicros = 50000

data QuotaSource
  = TierQuota
  | PassQuota Text (Maybe Text)

-- | Cancellation quota for a booking, or Nothing when the rider is not capped. Never throws.
getCancellationQuota :: (CacheFlow m r, EsqDBFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe CancellationQuota)
getCancellationQuota booking = do
  mbTierQuota <- tierCancellationQuota booking
  mbPass <- FRFSPassOverride.passForOverrideAppliedEntity booking.overrideAppliedEntityId
  let mbPassQuota = do
        entityId <- booking.overrideAppliedEntityId
        (payment, pass) <- mbPass
        limit <- pass.frfsCancelLimit
        tierQuota <- mbTierQuota
        guard (limit > 0)
        pure
          tierQuota
            { quotaKey = tierQuota.quotaKey <> ":purchasedPassPaymentId-" <> entityId,
              maxCancellations = limit,
              source = PassQuota entityId payment.passName
            }
  -- The pass quota borrows the tier's window, so an uncapped tier silently discards a pass limit
  -- that ops has explicitly set. Say so rather than leaving the rider quietly uncapped.
  when (isNothing mbPassQuota && isNothing mbTierQuota) $
    whenJust (mbPass >>= (.frfsCancelLimit) . snd) $ \limit ->
      when (limit > 0) $
        logWarning $
          "FRFS pass cancel limit " <> show limit <> " ignored: service tier has no cancellation quota configured, so the rider is uncapped. bookingId-" <> booking.id.getId
  pure (mbPassQuota <|> mbTierQuota)

tierCancellationQuota :: (CacheFlow m r, EsqDBFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe CancellationQuota)
tierCancellationQuota booking =
  case getServiceTierTypeFromRouteStationsJson booking.routeStationsJson of
    Nothing -> return Nothing
    Just serviceTierType -> do
      mbIntegratedBPPConfig <- SIBC.findByIdCP booking.integratedBppConfigId
      case mbIntegratedBPPConfig of
        Nothing -> return Nothing
        Just integratedBPPConfig -> do
          mbVst <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType booking.merchantOperatingCityId integratedBPPConfig.id
          let misconfigured reason = do
                logWarning $ "FRFS cancellation quota misconfigured for serviceTier-" <> show serviceTierType <> " mocId-" <> booking.merchantOperatingCityId.getId <> ": " <> reason <> ", treating as uncapped"
                return Nothing
          case (mbVst >>= (.maxCancellationCount), mbVst >>= (.cancellationWindowSeconds)) of
            (Nothing, Nothing) -> return Nothing
            (Just n, _) | n < 0 -> return Nothing
            (Just _, Nothing) -> misconfigured "maxCancellationCount is set but cancellationWindowSeconds is not"
            (Nothing, Just _) -> misconfigured "cancellationWindowSeconds is set but maxCancellationCount is not"
            (Just maxCancellations', Just windowSeconds')
              | maxCancellations' == 0 -> misconfigured "maxCancellationCount is 0 (use isCancellable to block cancellation)"
              | windowSeconds' <= 0 -> misconfigured ("cancellationWindowSeconds is " <> show windowSeconds')
              | otherwise ->
                return $
                  Just
                    CancellationQuota
                      { quotaKey = "FRFS:Cancel:Quota:v1:mocId-" <> booking.merchantOperatingCityId.getId <> ":personId-" <> booking.riderId.getId,
                        quotaMember = booking.id.getId,
                        maxCancellations = maxCancellations',
                        windowSeconds = windowSeconds',
                        source = TierQuota
                      }

-- | How many cancellations the rider has spent in the current window. Also repairs a key that lost
-- its expiry, which would otherwise block the rider forever.
getCancellationsUsed :: (Redis.HedisFlow m r, MonadFlow m) => CancellationQuota -> m Int
getCancellationsUsed quota = do
  members :: [Text] <- Redis.sMembers quota.quotaKey
  unless (null members) $ do
    remaining <- Redis.ttl quota.quotaKey
    when (remaining < 0) $ do
      logWarning $ "FRFS cancellation quota key has no expiry, repairing: " <> quota.quotaKey
      Redis.expire quota.quotaKey quota.windowSeconds
  return $ length members

-- | Record a completed cancellation. The TTL is set on the first hit and deliberately never
-- extended -- refreshing it would pin a frequent canceller at their ceiling forever.
-- Design notes: scripts/testing/cancel/DESIGN.md
markCancellationCounted :: (Redis.HedisFlow m r, MonadFlow m) => CancellationQuota -> m ()
markCancellationCounted quota = do
  Redis.sAdd quota.quotaKey [quota.quotaMember]
  remaining <- Redis.ttl quota.quotaKey
  when (remaining < 0) $ Redis.expire quota.quotaKey quota.windowSeconds

-- | Seconds until the allowance resets: the key expires wholesale, so this is its remaining TTL.
getRetryAfterSeconds :: (Redis.HedisFlow m r, MonadFlow m) => CancellationQuota -> m Int
getRetryAfterSeconds quota = do
  remaining <- Redis.ttl quota.quotaKey
  return $ if remaining > 0 then fromIntegral remaining else quota.windowSeconds

-- | Refuse the cancellation when the rider has spent their allowance.
-- Design notes: scripts/testing/cancel/DESIGN.md
checkCancellationQuota :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m ()
checkCancellationQuota booking = do
  mbQuota <- getCancellationQuota booking
  whenJust mbQuota $ \quota -> do
    used <- getCancellationsUsed quota
    when (used >= quota.maxCancellations) $ do
      retryAfter <- getRetryAfterSeconds quota
      logInfo $ "FRFS cancellation quota exhausted for riderId-" <> booking.riderId.getId <> " used: " <> show used <> " limit: " <> show quota.maxCancellations
      throwError $ FRFSCancellationLimitReached retryAfter

totalOrderValue :: MonadFlow m => DTBP.FRFSTicketBookingPaymentStatus -> DFRFSTicketBooking.FRFSTicketBooking -> m Price
totalOrderValue paymentBookingStatus booking =
  if paymentBookingStatus == DTBP.REFUND_PENDING || paymentBookingStatus == DTBP.REFUNDED
    then booking.totalPrice `addPrice` refundAmountToPrice -- Here the `refundAmountToPrice` value is in Negative
    else pure $ booking.totalPrice
  where
    refundAmountToPrice = mkPrice (Just INR) (fromMaybe (HighPrecMoney $ toRational (0 :: Int)) booking.refundAmount)

-- TODO :: This function called in Ticket Cancellation flow does not properly handle multiple quote category, whe enabling cancellation for multiple categories this needs to be rectified.
updateTotalOrderValueAndSettlementAmount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> BecknConfig -> m ()
updateTotalOrderValueAndSettlementAmount booking _quoteCategories bapConfig = do
  mbPaymentBooking <- runInReplica $ QFRFSTicketBookingPayment.findTicketBookingPayment booking
  unless (isJust mbPaymentBooking || FRFSPassOverride.isFullyPassCovered booking.overriddenAmount) $
    throwError (InvalidRequest "Payment booking not found for approved TicketBookingId")
  -- Divide by the number of recon rows, which is one per ticket the BPP issued -- NOT by the
  -- ticket quantity. buildReconTable splits the fare across `length tickets`, and an operator
  -- issuing one ticket for a multi-quantity booking is the common case, so dividing by quantity
  -- here rewrote a row holding the whole fare down to a fraction of it on every cancellation.
  ticketCount <- length <$> QFRFSTicket.findAllByTicketBookingId booking.id
  let reconRows = max 1 ticketCount
      finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      finderFeeForEachTicket = modifyPrice finderFee $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational reconRows)
  -- No payment row means pass-covered (the guard above), and that settles at FACE FARE, not at
  -- overriddenAmount: the operator is owed the fare for a real ride, already collected up front by
  -- the BUS_PASS recon row at pass purchase. See the same note in OnConfirm.
  tOrderPrice <- maybe (pure booking.totalPrice) (\paymentBooking -> totalOrderValue paymentBooking.status booking) mbPaymentBooking
  let tOrderValue = modifyPrice tOrderPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational reconRows)
  settlementAmount <- tOrderValue `subtractPrice` finderFeeForEachTicket
  void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById settlementAmount tOrderValue booking.id

counterCancellationRefundTag :: Text
counterCancellationRefundTag = "COUNTER_CANCELLATION_REFUND"

cancellationRefundTag :: Text
cancellationRefundTag = "CANCELLATION_REFUND"

-- | Books a counter-cancellation refund as one negative offsetting recon row per ticket, leaving the original rows' settlement untouched.
-- These rows enter the daily settlement/order book like any other recon entry (PENDING), so the clawback settles in the run of the day the on_cancel arrived, not the original booking's day.
createCancellationReconEntries ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DFRFSTicketStatus.FRFSTicketStatus ->
  Text ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  BecknConfig ->
  HighPrecMoney ->
  Maybe Text ->
  FRFSFareParameters ->
  m ()
createCancellationReconEntries reconTicketStatus reconMessage booking bapConfig refundAmount mRiderNumber fareParameters = do
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  unless (null tickets) $ do
    now <- getCurrentTime
    -- Negative, as `receiver_recon` expects a refund to read as a debit against the original order.
    let negatedRefundAmount = negate (abs refundAmount.getHighPrecMoney)
        orderRefundAmount = mkPrice Nothing $ HighPrecMoney negatedRefundAmount
        perTicketRefundAmount = mkPrice Nothing $ HighPrecMoney $ negatedRefundAmount / toRational (length tickets)
        finderFeeReturned = mkPrice Nothing $ HighPrecMoney 0
    reconEntries <- forM tickets $ \ticket -> do
      reconId <- generateGUID
      pure
        Recon.FRFSRecon
          { Recon.id = reconId,
            Recon.frfsTicketBookingId = booking.id,
            Recon.networkOrderId = fromMaybe "" booking.bppOrderId,
            Recon.collectorSubscriberId = bapConfig.subscriberId,
            Recon.receiverSubscriberId = booking.bppSubscriberId,
            Recon.date = show now,
            Recon.time = show now,
            Recon.mobileNumber = mRiderNumber,
            Recon.sourceStationCode = Just booking.fromStationCode,
            Recon.destinationStationCode = Just booking.toStationCode,
            Recon.ticketQty = Just fareParameters.totalQuantity,
            Recon.ticketNumber = Just ticket.ticketNumber,
            Recon.transactionRefNumber = booking.paymentTxnId,
            Recon.transactionUUID = Nothing,
            Recon.txnId = booking.paymentTxnId,
            Recon.fare = orderRefundAmount,
            Recon.buyerFinderFee = finderFeeReturned,
            Recon.totalOrderValue = perTicketRefundAmount,
            Recon.settlementAmount = perTicketRefundAmount,
            Recon.beneficiaryIFSC = booking.bppBankCode,
            Recon.beneficiaryBankAccount = booking.bppBankAccountNumber,
            Recon.collectorIFSC = bapConfig.bapIFSC,
            Recon.settlementReferenceNumber = Nothing,
            Recon.settlementDate = Nothing,
            Recon.differenceAmount = Nothing,
            Recon.message = Just reconMessage,
            Recon.ticketStatus = Just reconTicketStatus,
            Recon.providerId = booking.providerId,
            Recon.providerName = booking.providerName,
            Recon.entityType = Just Recon.FRFS_TICKET_BOOKING,
            Recon.reconStatus = Just Recon.PENDING,
            Recon.paymentGateway = Nothing,
            Recon.merchantId = Just booking.merchantId,
            Recon.merchantOperatingCityId = Just booking.merchantOperatingCityId,
            Recon.overrideType = booking.overrideType,
            Recon.overriddenAmount = booking.overriddenAmount,
            Recon.overrideAppliedEntityId = booking.overrideAppliedEntityId,
            Recon.createdAt = now,
            Recon.updatedAt = now
          }
    QFRFSRecon.createMany reconEntries

isOutsideBusinessHours :: Maybe Time.TimeOfDay -> Maybe Time.TimeOfDay -> UTCTime -> Seconds -> Bool
isOutsideBusinessHours startTime endTime now timeDiffFromUtc =
  case (startTime, endTime) of
    (Just start, Just end) -> isWithinTimeBound start end now timeDiffFromUtc
    _ -> False

isWithinTimeBound :: Time.TimeOfDay -> Time.TimeOfDay -> UTCTime -> Seconds -> Bool
isWithinTimeBound startTime endTime now timeDiffFromUtc =
  let tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
      nowAsLocal = Time.utcToLocalTime tz now
      nowTOD = Time.localTimeOfDay nowAsLocal

      --handle midnight wrap
      inWindow =
        if startTime <= endTime
          then nowTOD >= startTime && nowTOD <= endTime
          else nowTOD >= startTime || nowTOD <= endTime
   in inWindow

getQuantityTagFromCategory :: FRFSQuoteCategoryType -> FRFSCategorySpec.FRFSCategoryTag
getQuantityTagFromCategory categoryType = case categoryType of
  ADULT -> FRFSCategorySpec.ADULT_QUANTITY
  CHILD -> FRFSCategorySpec.CHILD_QUANTITY
  SENIOR_CITIZEN -> FRFSCategorySpec.SENIOR_CITIZEN_QUANTITY
  STUDENT -> FRFSCategorySpec.STUDENT_QUANTITY
  FEMALE -> FRFSCategorySpec.FEMALE_QUANTITY
  MALE -> FRFSCategorySpec.MALE_QUANTITY

getPriceTagFromCategory :: FRFSQuoteCategoryType -> FRFSCategorySpec.FRFSCategoryTag
getPriceTagFromCategory categoryType = case categoryType of
  ADULT -> FRFSCategorySpec.ADULT_PRICE
  CHILD -> FRFSCategorySpec.CHILD_PRICE
  SENIOR_CITIZEN -> FRFSCategorySpec.SENIOR_CITIZEN_PRICE
  STUDENT -> FRFSCategorySpec.STUDENT_PRICE
  FEMALE -> FRFSCategorySpec.FEMALE_PRICE
  MALE -> FRFSCategorySpec.MALE_PRICE

getTotalPriceTagFromCategory :: FRFSQuoteCategoryType -> FRFSCategorySpec.FRFSCategoryTag
getTotalPriceTagFromCategory categoryType = case categoryType of
  ADULT -> FRFSCategorySpec.TOTAL_ADULT_PRICE
  CHILD -> FRFSCategorySpec.TOTAL_CHILD_PRICE
  SENIOR_CITIZEN -> FRFSCategorySpec.TOTAL_SENIOR_CITIZEN_PRICE
  STUDENT -> FRFSCategorySpec.TOTAL_STUDENT_PRICE
  FEMALE -> FRFSCategorySpec.TOTAL_FEMALE_PRICE
  MALE -> FRFSCategorySpec.TOTAL_MALE_PRICE

data QuoteCategorySelection = QuoteCategorySelection
  { qcQuoteCategoryId :: Id DFRFSQuoteCategory.FRFSQuoteCategory,
    qcQuantity :: Int,
    qcSeatIds :: Maybe [Id DSeat.Seat],
    qcSeatLabels :: Maybe [Text]
  }

updateQuoteCategoriesWithSelections ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Text ->
  [QuoteCategorySelection] ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  m [DFRFSQuoteCategory.FRFSQuoteCategory]
updateQuoteCategoriesWithSelections mbHoldId selections quoteCategories = do
  mapM updateCategory quoteCategories
  where
    updateCategory category =
      case find (\sel -> sel.qcQuoteCategoryId == category.id) selections of
        Just sel -> do
          let updatedCategory =
                category
                  { DFRFSQuoteCategory.selectedQuantity = sel.qcQuantity,
                    DFRFSQuoteCategory.seatIds = sel.qcSeatIds,
                    DFRFSQuoteCategory.seatLabels = sel.qcSeatLabels,
                    DFRFSQuoteCategory.holdId = mbHoldId <|> category.holdId
                  }
          QFRFSQuoteCategory.updateByPrimaryKey updatedCategory
          return updatedCategory
        Nothing -> do
          let updatedCategory = category {DFRFSQuoteCategory.selectedQuantity = 0}
          QFRFSQuoteCategory.updateByPrimaryKey updatedCategory
          return updatedCategory

updateQuoteCategoriesWithFinalPrice ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [(Id DFRFSQuoteCategory.FRFSQuoteCategory, Price)] ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  m ([DFRFSQuoteCategory.FRFSQuoteCategory], Bool)
updateQuoteCategoriesWithFinalPrice categories quoteCategories = do
  updatedQuoteCategories <- mapM updateCategory quoteCategories
  let finalQuoteCategories = map fst updatedQuoteCategories
      isFareChanged = any (\(_, isFareChanged') -> isFareChanged') updatedQuoteCategories
  return (finalQuoteCategories, isFareChanged)
  where
    updateCategory category =
      case find (\(quoteCategoryId, _) -> quoteCategoryId == category.id) categories of
        Just (_, finalPrice) -> do
          QFRFSQuoteCategory.updateFinalPriceByQuoteCategoryId (Just finalPrice) category.id
          return (category {DFRFSQuoteCategory.finalPrice = Just finalPrice}, finalPrice /= category.offeredPrice)
        Nothing -> do
          QFRFSQuoteCategory.updateFinalPriceByQuoteCategoryId Nothing category.id
          return (category {DFRFSQuoteCategory.finalPrice = Nothing}, False)

createBasketFromBookings ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentServiceType ->
  Maybe Bool ->
  m [Payment.Basket]
createBasketFromBookings allJourneyBookings merchantId merchantOperatingCityId paymentServiceType mbEnableOffer = do
  logDebug $ "mbEnableOffer: " <> show mbEnableOffer
  let totalAmount = sum $ map (\booking -> fromMaybe booking.totalPrice.amount booking.overriddenAmount) allJourneyBookings
      dummyBasket =
        [ Payment.Basket
            { Payment.id = "no_basket",
              Payment.unitPrice = totalAmount,
              Payment.quantity = 1
            }
        ]
  if mbEnableOffer /= Just True
    then do
      return dummyBasket
    else do
      case allJourneyBookings of
        [booking] -> do
          -- offer valid only for single mode booking (not handled for multimodal right now)
          quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
          quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
          (mbAdultOfferSKUProductId', mbChildOfferSKUProductId') <- Payment.fetchOfferSKUConfig merchantId merchantOperatingCityId Nothing paymentServiceType
          let mbAdultOfferSKUProductId = Payment.substituteOverrideTypeInOfferSKU booking.overrideType booking.vehicleType booking.serviceTierType mbAdultOfferSKUProductId'
              mbChildOfferSKUProductId = Payment.substituteOverrideTypeInOfferSKU booking.overrideType booking.vehicleType booking.serviceTierType mbChildOfferSKUProductId'
          mbBenefit <- FRFSPassOverride.benefitForOverrideAppliedEntity booking.overrideAppliedEntityId
          let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
              applyBenefit price = maybe price (\benefit -> FRFSPassOverride.applyOverrideBenefit benefit price) mbBenefit
              skuForCategory category = case category of
                ADULT -> mbAdultOfferSKUProductId
                CHILD -> mbChildOfferSKUProductId
                _ -> Just ("no_basket_" <> T.toLower (show category))
              -- separate basket line per category, each keyed by its own offer SKU id
              mkBasket mbOfferSKUProductId mbQuantity mbUnitPrice =
                case (mbQuantity, mbUnitPrice) of
                  (Just quantity', Just unitPrice')
                    | quantity' > 0 ->
                      [ Payment.Basket
                          { Payment.id = fromMaybe "no_basket" mbOfferSKUProductId,
                            Payment.unitPrice = unitPrice',
                            Payment.quantity = quantity'
                          }
                      ]
                  _ -> []
              categoryItem category = find (\c -> c.categoryType == category) fareParameters.priceItems
              baskets
                | isJust booking.overrideType =
                  concatMap
                    ( \priceItem ->
                        mkBasket
                          (skuForCategory priceItem.categoryType)
                          (Just priceItem.quantity)
                          (Just (applyBenefit priceItem.unitPrice).amount)
                    )
                    fareParameters.priceItems
                | otherwise =
                  mkBasket mbAdultOfferSKUProductId (categoryItem ADULT <&> (.quantity)) (categoryItem ADULT <&> (.unitPrice.amount))
                    <> mkBasket mbChildOfferSKUProductId (categoryItem CHILD <&> (.quantity)) (categoryItem CHILD <&> (.unitPrice.amount))
              basketTotal = sum $ map (\b -> b.unitPrice * HighPrecMoney (toRational b.quantity)) baskets
          if null baskets
            then return dummyBasket
            else -- Checked against the amount the payment order actually charges, not against
            -- overriddenAmount directly. Today the two are the same -- overrideType and
            -- overriddenAmount are only ever written together, by FRFSConfirm on insert and by
            -- dropPassOverrideOnFareChange which clears all three -- so this is identical in
            -- behaviour. It stops being identical the moment a third writer sets one without the
            -- other: the basket would then be discounted by applyBenefit while the order charged
            -- face fare, and keying off overriddenAmount alone would skip the check precisely in
            -- that case.
            --
            -- Deliberately scoped to the override branch. The non-override basket only carries the
            -- ADULT and CHILD SKUs, so any further category -- or an unset CHILD SKU product id --
            -- makes basketTotal legitimately smaller than totalPrice, and checking it there would
            -- drop offer baskets for ordinary bookings.

              if isJust booking.overrideType && basketTotal /= fromMaybe booking.totalPrice.amount booking.overriddenAmount
                then do
                  logError $
                    "createBasketFromBookings: basket total does not match the charged amount, dropping offer basket bookingId="
                      <> booking.id.getId
                      <> " basketTotal="
                      <> show basketTotal
                      <> " overriddenAmount="
                      <> show booking.overriddenAmount
                      <> " totalPrice="
                      <> show booking.totalPrice.amount
                  return dummyBasket
                else return baskets
        _ -> return dummyBasket

-- TODO :: To be deprecated, and unified with SharedLogic.PaymentVendorSplits.createVendorSplit
createVendorSplitFromBookings ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentServiceType ->
  Bool ->
  m ([Payment.VendorSplitDetails], HighPrecMoney)
createVendorSplitFromBookings allJourneyBookings merchantId merchantOperatingCityId paymentType isFRFSTestingEnabled = do
  let amount =
        if isFRFSTestingEnabled
          then 1.0 * (HighPrecMoney $ toRational $ length allJourneyBookings)
          else
            foldl
              (\accAmt item -> accAmt + fromMaybe item.totalPrice.amount item.overriddenAmount)
              0.0
              allJourneyBookings
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentType
  case allJourneyBookings of
    [] -> return ([], 0.0)
    _ -> do
      if isSplitEnabled
        then do
          splitDetailsZippedByBooking <- do
            mapM
              ( \item -> do
                  integBppConfig <- SIBC.findIntegratedBPPConfigById item.integratedBppConfigId
                  vendorSplitDetailsList <- QVendorSplitDetails.findAllByIntegratedBPPConfigId integBppConfig.id
                  let amountPerBooking = if isFRFSTestingEnabled then 1.0 else fromMaybe item.totalPrice.amount item.overriddenAmount
                  return (item.id, (amountPerBooking, vendorSplitDetailsList))
              )
              allJourneyBookings
          vendorSplitDetailsListToIncludeInSplit <- QVendorSplitDetails.findAllByMerchantOperatingCityIdAndIncludeInSplit (Just merchantOperatingCityId) (Just True)
          vendorSplitDetails <- convertVendorDetails splitDetailsZippedByBooking vendorSplitDetailsListToIncludeInSplit isFRFSTestingEnabled
          return (vendorSplitDetails, amount)
        else return ([], amount)

convertVendorDetails ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r
  ) =>
  [(Id FTBooking.FRFSTicketBooking, (HighPrecMoney, [VendorSplitDetails.VendorSplitDetails]))] ->
  [VendorSplitDetails.VendorSplitDetails] ->
  Bool ->
  m [Payment.VendorSplitDetails]
convertVendorDetails splitDetailsZippedByBooking vendorDetailsToIncludeByDefault isFRFSTestingEnabled = do
  let validVendorSplitDetails = concat $ map (\ele -> createVendorSplitForBooking ele) splitDetailsZippedByBooking
  finalSplits <- ensureAllRequiredVendorsExist validVendorSplitDetails
  logInfo $ "validVendorSplitDetails" <> show validVendorSplitDetails
  logInfo $ "finalSplits" <> show finalSplits
  return finalSplits
  where
    createVendorSplitForBooking (bookingId, (amount, vd)) = map (\splitDetails -> toPaymentVendorDetails bookingId.getId amount splitDetails) vd
    toPaymentVendorDetails bookingId amount vd =
      let totalAmount = if isFRFSTestingEnabled then (1 :: HighPrecMoney) else amount
          splitAmount =
            if vd.splitType == VendorSplitDetails.FLEXIBLE
              then calculateSplitAmount vd.splitShare totalAmount
              else totalAmount
       in Payment.VendorSplitDetails
            { splitAmount = splitAmount,
              splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
              vendorId = vd.vendorId,
              ticketId = Just $ bookingId
            }

    calculateSplitAmount :: Maybe VendorSplitDetails.SplitShare -> HighPrecMoney -> HighPrecMoney
    calculateSplitAmount mbSplitPercentage totalAmount =
      case mbSplitPercentage of
        Just (VendorSplitDetails.Percentage percentage) ->
          totalAmount * (fromRational (toRational percentage) / 100.0)
        Just (VendorSplitDetails.FixedValue fixedValue) ->
          fromIntegral fixedValue
        Nothing ->
          totalAmount

    ensureAllRequiredVendorsExist ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      [Payment.VendorSplitDetails] ->
      m [Payment.VendorSplitDetails]
    ensureAllRequiredVendorsExist existingVendorSplits = do
      let existingVendorIds = map (.vendorId) existingVendorSplits
          missingVendors = filter (\vd -> vd.vendorId `notElem` existingVendorIds) vendorDetailsToIncludeByDefault
      missingVendorSplits <- mapM createDefaultVendorSplit missingVendors
      return $ existingVendorSplits ++ missingVendorSplits

    createDefaultVendorSplit ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      VendorSplitDetails.VendorSplitDetails ->
      m Payment.VendorSplitDetails
    createDefaultVendorSplit vd = do
      ticketId <- generateGUID
      return $
        Payment.VendorSplitDetails
          { splitAmount = 0,
            splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
            vendorId = vd.vendorId,
            ticketId = Just ticketId
          }

vendorSplitDetailSplitTypeToPaymentSplitType :: VendorSplitDetails.SplitType -> Payment.SplitType
vendorSplitDetailSplitTypeToPaymentSplitType = \case
  VendorSplitDetails.FIXED -> Payment.FIXED
  VendorSplitDetails.FLEXIBLE -> Payment.FLEXIBLE

mkCategoryInfoResponse :: DFRFSQuoteCategory.FRFSQuoteCategory -> APITypes.CategoryInfoResponse
mkCategoryInfoResponse category =
  APITypes.CategoryInfoResponse {categoryId = category.id, categoryName = category.category, categoryMeta = category.categoryMeta, categoryPrice = mkPriceAPIEntity category.price, categoryOfferedPrice = mkPriceAPIEntity category.offeredPrice, categoryFinalPrice = mkPriceAPIEntity <$> category.finalPrice, categorySelectedQuantity = category.selectedQuantity, seatIds = category.seatIds, seatLabels = category.seatLabels}

getPaymentType :: Bool -> Spec.VehicleCategory -> PaymentOrder.PaymentServiceType
getPaymentType isMultiModalBooking = \case
  Spec.METRO -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBooking
  Spec.SUBWAY -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBooking
  Spec.BUS -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBusBooking

unixToUTC :: Integer -> UTCTime
unixToUTC = posixSecondsToUTCTime . fromIntegral

getServiceTierTypeFromRouteStationsJson :: Maybe Text -> Maybe Spec.ServiceTierType
getServiceTierTypeFromRouteStationsJson mbJson = do
  rsJson <- mbJson
  (routeStations :: [APITypes.FRFSRouteStationsAPI]) <- decodeFromText rsJson
  firstRoute <- listToMaybe routeStations
  vst <- firstRoute.vehicleServiceTier
  Just vst._type

riderSpendKey :: Id DP.Person -> Text
riderSpendKey personId = "rider:spend:" <> personId.getId
