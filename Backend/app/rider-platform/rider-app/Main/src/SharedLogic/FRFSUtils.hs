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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import Domain.Types.AadhaarVerification as DAadhaarVerification
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
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
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
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
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import Storage.Queries.FRFSStageFare as QFRFSStageFare
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
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

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId merchantOperatingCityId vehicleType stationAPI = do
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
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
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
      possibleRoutes =
        nub $
          catMaybes $
            map
              ( \stops ->
                  let stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
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

data FRFSVehicleServiceTier = FRFSVehicleServiceTier
  { serviceTierType :: Spec.ServiceTierType,
    serviceTierProviderCode :: Text,
    serviceTierShortName :: Text,
    serviceTierDescription :: Text,
    serviceTierLongName :: Text
  }

data FRFSFare = FRFSFare
  { price :: Price,
    discounts :: [FRFSDiscount],
    vehicleServiceTier :: FRFSVehicleServiceTier
  }

getFares :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe (Id DP.Person) -> Spec.VehicleCategory -> Id IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFares mbRiderId vehicleType integratedBPPConfigId merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  currentTime <- getCurrentTime
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfigId
  let serviceableFareProducts = DTB.findBoundedDomain fareProducts currentTime ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
  mapM
    ( \fareProduct -> do
        vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
        farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
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
              let stage = abs $ endStageFare.stage - startStageFare.stage
              stageFare <- find (\stageFare -> stageFare.stage == stage) stageFares & fromMaybeM (InternalError "FRFS Stage Fare Not Found")
              return $
                Price
                  { amountInt = round stageFare.amount,
                    amount = stageFare.amount,
                    currency = stageFare.currency
                  }
        discountsWithEligibility <-
          case mbRiderId of
            Just riderId -> getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType riderId farePolicy.applicableDiscountIds
            Nothing -> pure []
        return $
          FRFSFare
            { price = price,
              discounts = map (mkDiscount price) discountsWithEligibility,
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
  { nextStop :: RouteStopMapping.RouteStopMapping,
    nextStopTravelTime :: Maybe Seconds,
    nextStopTravelDistance :: Meters,
    upcomingStops :: [UpcomingStop],
    vehicleId :: Text,
    vehicleInfo :: VehicleInfo,
    delay :: Maybe Seconds
  }
  deriving stock (Generic)
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
    tripId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

trackVehicles :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig]) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Spec.VehicleCategory -> Text -> m [VehicleTracking]
trackVehicles personId merchantId merchantOpCityId vehicleType routeCode = do
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  vehicleTrackingInfo <- getVehicleInfo integratedBPPConfig
  let vehicleInfoWithLatLong :: [(Text, VehicleInfo, Double, Double)] = mapMaybe (\(vId, vInfo) -> (vId,vInfo,,) <$> vInfo.latitude <*> vInfo.longitude) vehicleTrackingInfo
  routeStops <- QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id
  let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) routeStops
      stopPairs = pairWithNext sortedStops
  stopPairsWithWaypoints <- getStopPairsWithWaypoints stopPairs
  let segmentDistances = [(stopPair, (sum $ zipWith distanceBetweenInMeters waypoints (tail waypoints), duration)) | (stopPair, (waypoints, duration)) <- stopPairsWithWaypoints]
      segmentDistancesMap = Map.fromList segmentDistances
  forM vehicleInfoWithLatLong $ \(vehicleId, vehicleInfo, vehicleLat, vehicleLon) -> do
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
    let ((_, waypoints), nextStop) = minimumBy (comparing fst) minDistancesWithWaypoints
        nextStopTravelDistance = foldr (\(currPoint, nextPoint) distance -> distance + distanceBetweenInMeters currPoint nextPoint) (HighPrecMeters 0) (pairWithNext waypoints)
        nextStopTravelTime = (\speed -> Seconds $ round (nextStopTravelDistance.getHighPrecMeters / realToFrac speed)) <$> vehicleInfo.speed
        upcomingStopsList = dropWhile (/= nextStop) sortedStops
        upcomingStopPairs = pairWithNext upcomingStopsList

        (segmentDistancesForUpcoming, segmentDurationsForUpcoming :: [Maybe Seconds]) = unzip $ map (\pair -> fromMaybe (HighPrecMeters 0, Nothing) (Map.lookup pair segmentDistancesMap)) upcomingStopPairs

        cumulativeDistances = scanl (+) nextStopTravelDistance segmentDistancesForUpcoming

        upcomingStopsEntries =
          zipWith3
            ( \stop cumulativeDist duration -> do
                UpcomingStop
                  { stop = stop,
                    estimatedTravelTime = if isNothing duration then (\speed -> Seconds $ round (cumulativeDist.getHighPrecMeters / realToFrac speed)) <$> vehicleInfo.speed else duration, -- This is fallback value incase cached value is not available
                    actualTravelTime = if isNothing duration then (\speed -> Seconds $ round (cumulativeDist.getHighPrecMeters / realToFrac speed)) <$> vehicleInfo.speed else duration,
                    distance = highPrecMetersToMeters cumulativeDist
                  }
            )
            upcomingStopsList
            cumulativeDistances
            segmentDurationsForUpcoming
    currentTime <- liftIO getCurrentTime
    let delay
          | nextStopTravelDistance <= 100 =
            case (nextStopTravelTime, vehicleInfo.startTime) of
              (Just (Seconds estimatedSeconds), Just startTime) ->
                let actualDiff = diffUTCTime currentTime startTime
                    actualSeconds = fromIntegral (nominalDiffTimeToSeconds actualDiff)
                 in Just $ Seconds (actualSeconds - estimatedSeconds)
              _ -> Nothing
          | otherwise = Nothing

    let vehicleTracking =
          VehicleTracking
            { nextStopTravelDistance = highPrecMetersToMeters nextStopTravelDistance,
              upcomingStops = upcomingStopsEntries,
              ..
            }
    updatedVehicleTracking <- updateVehicleTrackingFromCache vehicleId vehicleTracking
    pure updatedVehicleTracking
  where
    updateVehicleTrackingFromCache vehicleId vehicleTracking = do
      mbCachedVehicleTracking <- Redis.safeGet (mkVehicleTrackingKey vehicleId routeCode)
      updatedTracking <- case mbCachedVehicleTracking of
        Just cachedVehicleTracking -> do
          let cachedStopMap =
                Map.fromList
                  [ (RouteStopMapping.stopCode (stop cachedStop), estimatedTravelTime cachedStop)
                    | cachedStop <- upcomingStops cachedVehicleTracking
                  ]
              updatedUpcomingStops =
                map
                  ( \newStop ->
                      case Map.lookup (RouteStopMapping.stopCode (stop newStop)) cachedStopMap of
                        Just cachedEstimatedTime -> newStop {estimatedTravelTime = cachedEstimatedTime}
                        Nothing -> newStop
                  )
                  (upcomingStops vehicleTracking)
              finalDelay = maybe cachedVehicleTracking.delay (\_ -> vehicleTracking.delay) vehicleTracking.delay
          pure $ vehicleTracking {upcomingStops = updatedUpcomingStops, delay = finalDelay}
        Nothing -> pure vehicleTracking
      Redis.set (mkVehicleTrackingKey vehicleId routeCode) updatedTracking
      pure updatedTracking

    getStopPairsWithWaypoints stopPairs = do
      Redis.get (stopPairRoutePointsKey routeCode)
        >>= \case
          Just stopPairsWithWaypointAndDuration -> pure stopPairsWithWaypointAndDuration
          Nothing -> do
            stopPairsWithWaypointAndDuration <-
              forM stopPairs $ \(currStop, nextStop) -> do
                let request =
                      Maps.GetRoutesReq
                        { waypoints = NE.fromList [currStop.stopPoint, nextStop.stopPoint],
                          calcPoints = True,
                          mode = Just Maps.CAR
                        }
                routeInfo <- Maps.getRoutes Nothing personId merchantId Nothing request
                reqRouteInfo <- listToMaybe routeInfo & fromMaybeM (RouteNotFound routeCode)
                let wayPoints = reqRouteInfo.points
                pure ((currStop, nextStop), (wayPoints, reqRouteInfo.duration :: Maybe Seconds))
            Redis.set (stopPairRoutePointsKey routeCode) stopPairsWithWaypointAndDuration
            pure stopPairsWithWaypointAndDuration

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
                      tripId = vehicleInfo.vehicleInfo.tripId
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

    stopPairRoutePointsKey :: Text -> Text
    stopPairRoutePointsKey routeId = "Tk:SPRPK:" <> routeId

mkVehicleTrackingKey :: Text -> Text -> Text
mkVehicleTrackingKey vehicleId routeCode = "FRFS:VehicleTracking:" <> vehicleId <> ":" <> routeCode

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
