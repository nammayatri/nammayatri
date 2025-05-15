{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSUtils where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.Aeson as A
import Data.List (groupBy, nub, sortBy)
import qualified Data.Text as T
import qualified Data.Time as Time
import Domain.Types.AadhaarVerification as DAadhaarVerification
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Domain.Types.RouteTripMapping as DRTM
import qualified Domain.Types.Station as Station
import EulerHS.Prelude (comparing, (+||), (||+))
import Kernel.Beam.Functions as B
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types ()
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import Storage.Beam.Yudhishthira ()
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import Storage.Queries.FRFSStageFare as QFRFSStageFare
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import Storage.Queries.IntegratedBPPConfig as IBC
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopFare as QRouteStopFare
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.RouteTripMapping as QRouteTripMapping
import Tools.DynamicLogic
import Tools.Error
import Tools.Maps as Maps

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- maybe (pure Nothing) (B.runInReplica . CQPOS.findByStationIdAndPOrgId id) mbPOrgId
  let pOrgStationName = pOrgStation <&> (.name)
  pure $ APITypes.FRFSStationAPI {name = fromMaybe name pOrgStationName, stationType = Nothing, color = Nothing, sequenceNum = Nothing, distance = Nothing, towards = Nothing, ..}

mkTBPStatusAPI :: DTBP.FRFSTicketBookingPaymentStatus -> APITypes.FRFSBookingPaymentStatusAPI
mkTBPStatusAPI = \case
  DTBP.PENDING -> APITypes.PENDING
  DTBP.SUCCESS -> APITypes.SUCCESS
  DTBP.FAILED -> APITypes.FAILURE
  DTBP.REFUND_PENDING -> APITypes.REFUND_PENDING
  DTBP.REFUNDED -> APITypes.REFUNDED

mkFRFSConfigAPI :: Config.FRFSConfig -> APITypes.FRFSConfigAPIRes
mkFRFSConfigAPI Config.FRFSConfig {..} = do
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, ..}

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> DIBC.PlatformType -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId merchantOperatingCityId vehicleType platformType stationAPI = do
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  station <- B.runInReplica $ CQS.findByStationCodeAndIntegratedBPPConfigId stationAPI.code integratedBPPConfig.id >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "and integratedBPPConfigId: " +|| integratedBPPConfig.id.getId ||+ "")
  mkPOrgStationAPIRes station mbPOrgId

data FRFSTicketDiscountDynamic = FRFSTicketDiscountDynamic
  { aadhaarData :: Maybe DAadhaarVerification.AadhaarVerification,
    discounts :: [DFRFSTicketDiscount.FRFSTicketDiscount]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

getFRFSTicketDiscountWithEligibility ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  Id DP.Person ->
  [Id FRFSTicketDiscount] ->
  m [(FRFSTicketDiscount, Bool)]
getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType personId applicableDiscountIds = do
  availableDiscounts <-
    pure . catMaybes
      =<< mapM
        ( \applicableDiscountId -> QFRFSTicketDiscount.findByIdAndVehicleAndCity applicableDiscountId vehicleType merchantId merchantOperatingCityId
        )
        applicableDiscountIds
  aadhaarVerification <- QAV.findByPersonId personId
  applicableDiscounts <- do
    let ticketDiscountData = FRFSTicketDiscountDynamic {aadhaarData = aadhaarVerification, discounts = availableDiscounts}
    localTime <- getLocalCurrentTime 19800 -- Fix Me
    (allLogics, _) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_DISCOUNTS localTime Nothing Nothing
    response <- try @_ @SomeException $ LYTU.runLogics allLogics ticketDiscountData
    case response of
      Left e -> do
        logError $ "Error in running FRFS Discount Logic - " <> show e <> " - " <> show ticketDiscountData <> " - " <> show allLogics
        return []
      Right resp ->
        case (A.fromJSON resp.result :: Result FRFSTicketDiscountDynamic) of
          A.Success result -> return result.discounts
          A.Error err -> do
            logError $ "Error in parsing FRFSTicketDiscountDynamic - " <> show err <> " - " <> show resp <> " - " <> show ticketDiscountData <> " - " <> show allLogics
            return []
  return $ mergeDiscounts availableDiscounts applicableDiscounts
  where
    mergeDiscounts availableDiscounts applicableDiscounts =
      map (\discount -> (discount, discount `elem` applicableDiscounts)) availableDiscounts

data RouteStopInfo = RouteStopInfo
  { route :: Route.Route,
    startStopCode :: Text,
    endStopCode :: Text,
    totalStops :: Maybe Int,
    stops :: Maybe [RouteStopMapping.RouteStopMapping],
    travelTime :: Maybe Seconds
  }

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> IntegratedBPPConfig -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig = do
  routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStationCode integratedBPPConfig.id
  let routeCodes = nub $ map (.routeCode) routesWithStop
  let integratedBPPConfigIds = replicate (length routeCodes) (integratedBPPConfig.id)
  routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes integratedBPPConfigIds
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
  let integratedBPPConfigIds' = replicate (length mappedRouteCodes) (integratedBPPConfig.id)
  routes <- QRoute.findByRouteCodes mappedRouteCodes integratedBPPConfigIds'
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

data FRFSDiscount = FRFSDiscount
  { code :: Text,
    title :: Text,
    description :: Text,
    tnc :: Text,
    price :: Price,
    eligibility :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FRFSVehicleServiceTier = FRFSVehicleServiceTier
  { serviceTierType :: Spec.ServiceTierType,
    serviceTierProviderCode :: Text,
    serviceTierShortName :: Text,
    serviceTierDescription :: Text,
    serviceTierLongName :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FRFSFare = FRFSFare
  { price :: Price,
    childPrice :: Maybe Price,
    discounts :: [FRFSDiscount],
    fareDetails :: Maybe Quote.FRFSFareDetails,
    vehicleServiceTier :: FRFSVehicleServiceTier
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getFares :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Spec.VehicleCategory -> Id IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFares riderId vehicleType integratedBPPConfigId merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  currentTime <- getCurrentTime
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfigId
  let serviceableFareProducts = DTB.findBoundedDomain fareProducts currentTime ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
  mapM
    ( \fareProduct -> do
        vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
        farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
        let cessCharge = fromMaybe (HighPrecMoney 0) farePolicy.cessCharge
        price <-
          case farePolicy._type of
            DFRFSFarePolicy.MatrixBased -> do
              routeStopFare <- QRouteStopFare.findByRouteStartAndStopCode farePolicy.id routeCode startStopCode endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Fare Not Found")
              return $
                Price
                  { amountInt = round routeStopFare.amount,
                    amount = routeStopFare.amount,
                    currency = routeStopFare.currency
                  }
            DFRFSFarePolicy.StageBased -> do
              stageFares <- QFRFSStageFare.findAllByFarePolicyId farePolicy.id
              startStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeCode startStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
              endStageFare <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicy.id routeCode endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Stage Fare Not Found")
              let stage = max 1 (abs $ endStageFare.stage - startStageFare.stage) -- if stage is 0, then it is the same stage so we take 1 as the stage
              stageFare <- find (\stageFare -> stageFare.stage == stage) stageFares & fromMaybeM (InternalError "FRFS Stage Fare Not Found")
              let amount = stageFare.amount + cessCharge
              return $
                Price
                  { amountInt = round amount,
                    amount = amount,
                    currency = stageFare.currency
                  }
        discountsWithEligibility <- getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType riderId farePolicy.applicableDiscountIds
        return $
          FRFSFare
            { price = price,
              childPrice = Nothing,
              discounts = map (mkDiscount price) discountsWithEligibility,
              fareDetails = Nothing,
              vehicleServiceTier =
                FRFSVehicleServiceTier
                  { serviceTierType = vehicleServiceTier._type,
                    serviceTierProviderCode = vehicleServiceTier.providerCode,
                    serviceTierShortName = vehicleServiceTier.shortName,
                    serviceTierDescription = vehicleServiceTier.description,
                    serviceTierLongName = vehicleServiceTier.longName
                  }
            }
    )
    serviceableFareProducts
  where
    mkDiscount price (discount, eligibility) =
      let discountPrice =
            case discount.value of
              DFRFSTicketDiscount.FixedAmount amount ->
                Price
                  { amountInt = round amount,
                    amount = amount,
                    currency = discount.currency
                  }
              DFRFSTicketDiscount.Percentage percent ->
                Price
                  { amountInt = round ((HighPrecMoney (toRational percent) * price.amount) / 100),
                    amount = (HighPrecMoney (toRational percent) * price.amount) / 100,
                    currency = discount.currency
                  }
       in FRFSDiscount
            { code = discount.code,
              title = discount.title,
              description = discount.description,
              tnc = discount.tnc,
              price = discountPrice,
              ..
            }

data VehicleTracking = VehicleTracking
  { nextStop :: Maybe RouteStopMapping.RouteStopMapping,
    nextStopTravelTime :: Maybe Seconds,
    nextStopTravelDistance :: Maybe Meters,
    upcomingStops :: [UpcomingStop],
    vehicleId :: Text,
    vehicleInfo :: Maybe VehicleInfo,
    delay :: Maybe Seconds
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpcomingStop = UpcomingStop
  { stop :: RouteStopMapping.RouteStopMapping,
    estimatedTravelTime :: Maybe Seconds,
    actualTravelTime :: Maybe Seconds,
    distance :: Meters
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo
  { latitude :: Maybe Double,
    longitude :: Maybe Double,
    scheduleRelationship :: Maybe Text,
    speed :: Maybe Double,
    startDate :: Maybe Text,
    startTime :: Maybe UTCTime,
    timestamp :: Maybe Text,
    tripId :: Maybe Text,
    upcomingStops :: Maybe [LT.UpcomingStop]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

trackVehicles :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Maybe LatLong -> m [VehicleTracking]
trackVehicles _personId _merchantId merchantOpCityId vehicleType routeCode platformType mbRiderPosition = do
  now <- getCurrentTime
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case vehicleType of
    Spec.BUS -> do
      case platformType of
        DIBC.APPLICATION -> do
          vehicleTrackingInfo <- getVehicleInfo integratedBPPConfig
          mapM
            ( \(vehicleId, vehicleInfo) -> do
                upcomingStop <-
                  case vehicleInfo.upcomingStops of
                    Just upcomingStops -> do
                      let mbUpcomingStop = find (\upcomingStop -> upcomingStop.status == LT.Upcoming) upcomingStops
                      case mbUpcomingStop of
                        Just upcomingStop' -> listToMaybe <$> QRouteStopMapping.findByRouteCodeAndStopCode routeCode upcomingStop'.stop.stopCode integratedBPPConfig.id
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
                      delay = Nothing
                    }
            )
            vehicleTrackingInfo
        _ -> do
          nearbyBuses <- CQMMB.getRoutesBuses routeCode -- Add a new logic to get the bus location and ETA, unify it with the existing logic @khuzema
          logDebug $ "Got bus data for route " <> routeCode <> ": " <> show nearbyBuses
          nearbyBuses.buses `forM` \bus -> do
            let busData = bus.busData
            let sortedEtaData = sortBy (comparing (.stopSeq)) (fromMaybe [] busData.eta_data)
            let mbNextStop = listToMaybe sortedEtaData
            logDebug $ "Got bus data for route " <> routeCode <> ": next stop" <> show mbNextStop
            mbNextStopMapping <-
              case mbNextStop of
                Just nextStop -> do
                  logDebug $ "Got bus data for route " <> routeCode <> ": next stop mapping" <> show nextStop <> " data: " <> show routeCode <> " " <> nextStop.stopId <> " " <> integratedBPPConfig.id.getId
                  listToMaybe <$> QRouteStopMapping.findByRouteCodeAndStopCode routeCode nextStop.stopId integratedBPPConfig.id
                Nothing -> pure Nothing
            return $
              VehicleTracking
                { nextStop = mbNextStopMapping,
                  nextStopTravelTime = (\t -> nominalDiffTimeToSeconds $ diffUTCTime t now) <$> (mbNextStop <&> (.arrivalTime)),
                  nextStopTravelDistance = Nothing,
                  upcomingStops = [], -- fix it later
                  vehicleId = bus.vehicleNumber,
                  vehicleInfo =
                    Just $
                      VehicleInfo
                        { latitude = Just busData.latitude,
                          longitude = Just busData.longitude,
                          scheduleRelationship = Nothing,
                          speed = Nothing,
                          startDate = Nothing,
                          startTime = Nothing,
                          timestamp = Nothing,
                          tripId = Nothing,
                          upcomingStops = Nothing
                        },
                  delay = Nothing
                }
    _ -> do
      route <- QRoute.findByRouteCode routeCode integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode)
      routeStops <- QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id

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

    getVehicleInfo integratedBPPConfig = do
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
                      upcomingStops = vehicleInfo.vehicleInfo.upcomingStops
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

mergeFFRFSRouteDetails :: [FRFSRouteDetails] -> Maybe FRFSRouteDetails
mergeFFRFSRouteDetails routeDetails = do
  let mbFirstRouteDetails = listToMaybe routeDetails
  let mbLastRouteDetails = listToMaybe (reverse routeDetails)
  case (mbFirstRouteDetails, mbLastRouteDetails) of
    (Just firstRouteDetails, Just lastRouteDetails) ->
      Just $
        FRFSRouteDetails
          { routeCode = firstRouteDetails.routeCode,
            startStationCode = firstRouteDetails.startStationCode,
            endStationCode = lastRouteDetails.endStationCode
          }
    _ -> Nothing

getRouteByStationIdsAndIntegratedBPPConfigId :: (CacheFlow m r, EsqDBFlow m r) => Id Station.Station -> Id Station.Station -> Maybe (Id DIBC.IntegratedBPPConfig) -> m (Maybe RouteStopInfo)
getRouteByStationIdsAndIntegratedBPPConfigId fromStationId toStationId integratedBPPConfigId = do
  fromStation <- CQS.findById fromStationId >>= fromMaybeM (StationNotFound $ "from station not found with id: " +|| fromStationId.getId ||+ "")
  toStation <- CQS.findById toStationId >>= fromMaybeM (StationNotFound $ "from station not found with id: " +|| toStationId.getId ||+ "")
  integratedBPPConfig <- case (integratedBPPConfigId) of
    Just integratedBppConfigId -> do
      IBC.findById integratedBppConfigId >>= fromMaybeM (InvalidRequest "integratedBppConfig not found")
    Nothing -> do
      QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) fromStation.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory fromStation.vehicleType) DIBC.APPLICATION
        >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| fromStation.merchantOperatingCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory fromStation.vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  routeInfo' <- getPossibleRoutesBetweenTwoStops fromStation.code toStation.code integratedBPPConfig
  let routeInfo = listToMaybe routeInfo'
  case routeInfo of
    Just a -> pure $ Just a
    Nothing -> pure Nothing

partnerOrgRiderId :: Id DP.Person
partnerOrgRiderId = Id "partnerOrg_rider_id"

partnerOrgBppItemId :: Text
partnerOrgBppItemId = "partnerOrg_bpp_item_id"

partnerOrgBppSubscriberId :: Text
partnerOrgBppSubscriberId = "partnerOrg_bpp_subscriber_id"

partnerOrgBppSubscriberUrl :: Text
partnerOrgBppSubscriberUrl = "partnerOrg_bpp_subscriber_url"
