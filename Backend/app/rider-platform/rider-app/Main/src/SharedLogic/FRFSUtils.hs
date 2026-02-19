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
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Types.AadhaarVerification as DAadhaarVerification
import Domain.Types.BecknConfig
import qualified Domain.Types.Extra.VendorSplitDetails as VendorSplitDetails
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategorySpec as FRFSCategorySpec
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteFareProduct
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.FRFSTicketBookingPaymentCategory as DTBPC
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig as DFRFSTicketCategoryMetadataConfig
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
import qualified Domain.Types.Station as Station
import qualified Domain.Types.VendorSplitDetails as VendorSplitDetails
import EulerHS.Prelude (comparing, concatMapM, (+||), (||+))
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
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSFareCalculator as Reexport
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSGtfsStageFare as QFRFSGtfsStageFare
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
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBookingPaymentCategory as QFRFSTicketBookingPaymentCategory
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.JourneyLeg as QJL
import Storage.Queries.RouteTripMapping as QRouteTripMapping
import Storage.Queries.StopFare as QRouteStopFare
import qualified Storage.Queries.VendorSplitDetails as QVendorSplitDetails
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Payment as Payment
import qualified Tools.Wallet as TWallet

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

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- maybe (pure Nothing) (\pOrgId -> CQPOS.findByStationCodeAndPOrgId code pOrgId |<|>| CQPOS.findByStationCodeAndPOrgId id.getId pOrgId) mbPOrgId
  let pOrgStationName = pOrgStation <&> (.name)
  pure $ APITypes.FRFSStationAPI {name = Just $ fromMaybe name pOrgStationName, routeCodes = Nothing, stationType = Nothing, color = Nothing, sequenceNum = Nothing, distance = Nothing, towards = Nothing, timeTakenToTravelUpcomingStop = Nothing, ..}

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
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, ..}

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
                fares
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

trackVehicles :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Maybe LatLong -> Maybe (Id DIBC.IntegratedBPPConfig) -> m [VehicleTracking]
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

createPaymentOrder ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  Id DMOC.MerchantOperatingCity ->
  Id Merchant.Merchant ->
  HighPrecMoney ->
  DP.Person ->
  Payment.PaymentServiceType ->
  [Payment.VendorSplitDetails] ->
  Maybe [Payment.Basket] ->
  Bool ->
  m (Maybe DOrder.PaymentOrder)
createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr basket isMockPayment = do
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
            splitSettlementDetails = splitSettlementDetails,
            basket = basket
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
  mapM (\resp -> DPayment.buildPaymentOrder commonMerchantId (Just commonMerchantOperatingCityId) commonPersonId mbPaymentOrderValidTill Nothing paymentType createOrderReq resp isMockPayment groupId) orderResp
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
            updatedAt = now
          }

makecancelledTtlKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
makecancelledTtlKey bookingId = "FRFS:OnConfirm:CancelledTTL:bookingId-" <> bookingId.getId

totalOrderValue :: MonadFlow m => DTBP.FRFSTicketBookingPaymentStatus -> DFRFSTicketBooking.FRFSTicketBooking -> m Price
totalOrderValue paymentBookingStatus booking =
  if paymentBookingStatus == DTBP.REFUND_PENDING || paymentBookingStatus == DTBP.REFUNDED
    then booking.totalPrice `addPrice` refundAmountToPrice -- Here the `refundAmountToPrice` value is in Negative
    else pure $ booking.totalPrice
  where
    refundAmountToPrice = mkPrice (Just INR) (fromMaybe (HighPrecMoney $ toRational (0 :: Int)) booking.refundAmount)

-- TODO :: This function called in Ticket Cancellation flow does not properly handle multiple quote category, whe enabling cancellation for multiple categories this needs to be rectified.
updateTotalOrderValueAndSettlementAmount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> BecknConfig -> m ()
updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig = do
  paymentBooking <- runInReplica $ QFRFSTicketBookingPayment.findTicketBookingPayment booking >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
      finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      finderFeeForEachTicket = modifyPrice finderFee $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational fareParameters.totalQuantity)
  tOrderPrice <- totalOrderValue paymentBooking.status booking
  let tOrderValue = modifyPrice tOrderPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational fareParameters.totalQuantity)
  settlementAmount <- tOrderValue `subtractPrice` finderFeeForEachTicket
  void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById settlementAmount tOrderValue booking.id

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

updateQuoteCategoriesWithQuantitySelections ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [(Id DFRFSQuoteCategory.FRFSQuoteCategory, Int)] ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  m [DFRFSQuoteCategory.FRFSQuoteCategory]
updateQuoteCategoriesWithQuantitySelections categories quoteCategories = do
  updatedQuoteCategories <- mapM updateCategory quoteCategories
  return updatedQuoteCategories
  where
    updateCategory category =
      case find (\(quoteCategoryId, _) -> quoteCategoryId == category.id) categories of
        Just (_, quantity) -> do
          QFRFSQuoteCategory.updateQuantityByQuoteCategoryId quantity category.id
          return (category {DFRFSQuoteCategory.selectedQuantity = quantity})
        Nothing -> do
          QFRFSQuoteCategory.updateQuantityByQuoteCategoryId 0 category.id
          return (category {DFRFSQuoteCategory.selectedQuantity = 0})

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
  let dummyBasket =
        [ Payment.Basket
            { Payment.id = "no_basket",
              Payment.unitPrice = 0,
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
          mbOfferSKUProductId <- Payment.fetchOfferSKUConfig merchantId merchantOperatingCityId Nothing paymentServiceType
          let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
              adultQuantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity)
              childQuantity = find (\category -> category.categoryType == CHILD) fareParameters.priceItems <&> (.quantity)
              adultUnitPrice = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice.amount)
              childUnitPrice = find (\category -> category.categoryType == CHILD) fareParameters.priceItems <&> (.unitPrice.amount)
          case (mbOfferSKUProductId, adultQuantity, childQuantity, adultUnitPrice, childUnitPrice) of
            (Just offerSKUProductId, Just adultQuantity', childQuantity', Just adultUnitPrice', _) -> do
              if adultQuantity' == 1 && fromMaybe 0 childQuantity' == 0
                then
                  return $
                    [ Payment.Basket
                        { Payment.id = offerSKUProductId,
                          Payment.unitPrice = adultUnitPrice',
                          Payment.quantity = adultQuantity'
                        }
                    ]
                else return dummyBasket
            _ -> return dummyBasket
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
              (\accAmt item -> accAmt + item.totalPrice.amount)
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
                  let amountPerBooking = if isFRFSTestingEnabled then 1.0 else item.totalPrice.amount
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
  APITypes.CategoryInfoResponse {categoryId = category.id, categoryName = category.category, categoryMeta = category.categoryMeta, categoryPrice = mkPriceAPIEntity category.price, categoryOfferedPrice = mkPriceAPIEntity category.offeredPrice, categoryFinalPrice = mkPriceAPIEntity <$> category.finalPrice, categorySelectedQuantity = category.selectedQuantity}

getPaymentType :: Bool -> Spec.VehicleCategory -> PaymentOrder.PaymentServiceType
getPaymentType isMultiModalBooking = \case
  Spec.METRO -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBooking
  Spec.SUBWAY -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBooking
  Spec.BUS -> if isMultiModalBooking then PaymentOrder.FRFSMultiModalBooking else PaymentOrder.FRFSBusBooking
