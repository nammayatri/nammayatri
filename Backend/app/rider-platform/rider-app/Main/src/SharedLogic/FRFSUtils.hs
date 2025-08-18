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
import Control.Monad.Extra (mapMaybeM)
import Data.Aeson as A
import Data.List (groupBy, nub, sortBy)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Types.AadhaarVerification as DAadhaarVerification
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSFarePolicy as DFRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.FRFSRouteFareProduct
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
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
import EulerHS.Prelude (comparing, concatMapM, (+||), (||+))
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types ()
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import Lib.Payment.Domain.Types.Refunds as Refunds
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CreateFareForMultiModal as SMMFRFS
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler as JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.FRFSGtfsStageFare as QFRFSGtfsStageFare
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import Storage.CachedQueries.RouteStopTimeTable as QRouteStopTimeTable
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import Storage.Queries.FRFSStageFare as QFRFSStageFare
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJL
import qualified Storage.Queries.Person as QP
import Storage.Queries.RouteTripMapping as QRouteTripMapping
import Storage.Queries.StopFare as QRouteStopFare
import Tools.DynamicLogic
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Payment as Payment

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

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => Text -> Text -> IntegratedBPPConfig -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig = do
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
  { farePolicyId :: Maybe (Id DFRFSFarePolicy.FRFSFarePolicy),
    price :: Price,
    childPrice :: Maybe Price,
    discounts :: [FRFSDiscount],
    fareDetails :: Maybe Quote.FRFSFareDetails,
    vehicleServiceTier :: FRFSVehicleServiceTier
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getFare :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Spec.VehicleCategory -> Id IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFare riderId vehicleType integratedBPPConfigId merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  now <- getCurrentTime
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfigId
  let serviceableFareProducts = DTB.findBoundedDomain fareProducts now ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
  mapM (buildFRFSFare riderId vehicleType merchantId merchantOperatingCityId routeCode startStopCode endStopCode) serviceableFareProducts

buildFRFSFare :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Spec.VehicleCategory -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> FRFSRouteFareProduct -> m FRFSFare
buildFRFSFare riderId vehicleType merchantId merchantOperatingCityId routeCode startStopCode endStopCode fareProduct = do
  vehicleServiceTier <- QFRFSVehicleServiceTier.findById fareProduct.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fareProduct.vehicleServiceTierId.getId)
  farePolicy <- QFRFSFarePolicy.findById fareProduct.farePolicyId >>= fromMaybeM (InternalError $ "FRFS Fare Policy Not Found : " <> fareProduct.farePolicyId.getId)
  let cessCharge = fromMaybe (HighPrecMoney 0) farePolicy.cessCharge
  price <-
    case farePolicy._type of
      DFRFSFarePolicy.MatrixBased -> do
        routeStopFare <- QRouteStopFare.findByRouteStartAndStopCode farePolicy.id startStopCode endStopCode >>= fromMaybeM (InternalError "FRFS Route Stop Fare Not Found")
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
      { farePolicyId = Just farePolicy.id,
        price = price,
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

getCachedRouteStopFares :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DP.Person -> Spec.VehicleCategory -> IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getCachedRouteStopFares riderId vehicleType integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  routeStopFare <- QRouteStopFare.findByStartAndEndStopCodeAndIntegratedBPPConfigId startStopCode endStopCode integratedBPPConfig.id
  case routeStopFare of
    Just fare -> do
      currentTime <- getCurrentTime
      fareProducts <- QFRFSRouteFareProduct.findAllByFarePoliyIdAndIntegratedBPPConfigId fare.farePolicyId integratedBPPConfig.id
      let serviceableFareProducts = DTB.findBoundedDomain fareProducts currentTime ++ filter (\fareProduct -> fareProduct.timeBounds == DTB.Unbounded) fareProducts
      mapM (buildFRFSFare riderId vehicleType merchantId merchantOperatingCityId routeCode startStopCode endStopCode) serviceableFareProducts
    Nothing -> return []

mkDiscount :: Price -> (FRFSTicketDiscount, Bool) -> FRFSDiscount
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

getFareThroughGTFS :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DP.Person -> Spec.VehicleCategory -> IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFareThroughGTFS riderId vehicleType integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  routeStopTimeTableStartStop <- listToMaybe <$> QRouteStopTimeTable.findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOperatingCityId [routeCode] startStopCode
  routeStopTimeTableEndStop <- listToMaybe <$> QRouteStopTimeTable.findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOperatingCityId [routeCode] endStopCode
  logDebug $ "routeStopTimeTableStartStop: " <> show routeStopTimeTableStartStop <> " routeStopTimeTableEndStop: " <> show routeStopTimeTableEndStop
  case (routeStopTimeTableStartStop, routeStopTimeTableEndStop) of
    (Just startStop, Just endStop) -> do
      case (startStop.stage, endStop.stage) of
        (Just startStage, Just endStage) -> do
          let stage = abs (endStage - startStage)
          logDebug $ "isStageStop flags: startStop=" <> show startStop.isStageStop <> " endStop=" <> show endStop.isStageStop
          let adjustedStage = case endStop.isStageStop of
                Just True -> stage - 1 -- Reduce stage by 1 if found, but ensure minimum is 1
                _ -> stage -- Use original stage if not found or Nothing
          fares <- QFRFSGtfsStageFare.findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType (max 0 adjustedStage) merchantOperatingCityId
          forM fares $ \fare -> do
            vehicleServiceTier <- QFRFSVehicleServiceTier.findById fare.vehicleServiceTierId >>= fromMaybeM (InternalError $ "FRFS Vehicle Service Tier Not Found " <> fare.vehicleServiceTierId.getId)
            let price = Price {amountInt = round (fare.amount + fromMaybe 0 fare.cessCharge), amount = fare.amount + fromMaybe 0 fare.cessCharge, currency = fare.currency}
            discountsWithEligibility <- getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType riderId fare.discountIds
            logDebug $ "discountsWithEligibility: " <> show discountsWithEligibility <> " fare: " <> show fare <> " price: " <> show price <> " vehicleServiceTier: " <> show vehicleServiceTier <> " fare.discountIds: "
            return $
              FRFSFare
                { farePolicyId = Nothing,
                  price = price,
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
        _ -> return []
    _ -> return []

getFares :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id DP.Person -> Spec.VehicleCategory -> IntegratedBPPConfig -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Text -> m [FRFSFare]
getFares riderId vehicleType integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode = do
  fares <- getFareThroughGTFS riderId vehicleType integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
  if null fares
    then do
      try @_ @SomeException (getFare riderId vehicleType integratedBPPConfig.id merchantId merchantOperatingCityId routeCode startStopCode endStopCode)
        >>= \case
          Left err -> do
            logError $ "Error in getFare: " <> show err
            return []
          Right fares' ->
            case integratedBPPConfig.providerConfig of
              DIBC.ONDC DIBC.ONDCBecknConfig {fareCachingAllowed} -> do
                if null fares' && (fareCachingAllowed == Just True)
                  then do
                    try @_ @SomeException (getCachedRouteStopFares riderId vehicleType integratedBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode)
                      >>= \case
                        Left err -> do
                          logError $ "Error in getCachedRouteStopFares: " <> show err
                          return fares'
                        Right cachedFares -> return cachedFares
                  else return fares'
              _ -> return fares'
    else return fares

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
          vehicleTrackingInfo <- getVehicleInfo integratedBPPConfig
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
            let (_, upcomingStops) =
                  foldr'
                    ( \stop (lastPoint, acc) -> do
                        let us =
                              UpcomingStop
                                { stopCode = stop.stopCode,
                                  stopSeq = stop.stopSeq,
                                  stopName = stop.stopName,
                                  estimatedTravelTime = Just stop.arrivalTime,
                                  travelDistance = fmap highPrecMetersToMeters (\lastPoint' -> distanceBetweenInMeters lastPoint' (mkLatLong stop.stopLat stop.stopLon)) <$> lastPoint,
                                  actualTravelTime = Nothing
                                }
                        (Just (mkLatLong stop.stopLat stop.stopLon), us : acc)
                    )
                    (mbRiderPosition, [])
                    sortedEtaData
            logDebug $ "Got bus data for route " <> routeCode <> ": next stop" <> show mbNextStop
            mbNextStopMapping <-
              case mbNextStop of
                Just nextStop -> do
                  logDebug $ "Got bus data for route " <> routeCode <> ": next stop mapping" <> show nextStop <> " data: " <> show routeCode <> " " <> nextStop.stopCode <> " " <> integratedBPPConfig.id.getId
                  nextStop' <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode nextStop.stopCode routeCode integratedBPPConfig
                  return $ listToMaybe nextStop'
                Nothing -> pure Nothing
            return $
              VehicleTracking
                { nextStop = mbNextStopMapping,
                  nextStopTravelTime = (\t -> nominalDiffTimeToSeconds $ diffUTCTime t now) <$> (mbNextStop <&> (.arrivalTime)),
                  nextStopTravelDistance = Nothing,
                  upcomingStops = upcomingStops, -- fix it later
                  vehicleId = bus.vehicleNumber,
                  vehicleInfo =
                    Just $
                      VehicleInfo
                        { latitude = Just busData.latitude,
                          longitude = Just busData.longitude,
                          scheduleRelationship = Nothing,
                          speed = Just busData.speed,
                          startDate = Nothing,
                          startTime = Nothing,
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
      legsWithMapping <- QJL.getJourneyLegs leg.journeyId
      let legs = map fst legsWithMapping
      bookings <- mapMaybeM (QFRFSTicketBooking.findBySearchId . Id) (mapMaybe (.legSearchId) legs)
      return (Just leg.journeyId, bookings)
    Nothing -> pure (Nothing, [booking])

markAllRefundBookings ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    SchedulerFlow r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  Id DP.Person ->
  m ()
markAllRefundBookings booking personId = do
  (mbJourneyId, allJourneyFrfsBookings) <- getAllJourneyFrfsBookings booking
  allPaymentBookings <- mapM (QFRFSTicketBookingPayment.findNewTBPByBookingId . (.id)) allJourneyFrfsBookings
  let paymentBookings = catMaybes allPaymentBookings

  let terminalBookings = filter (\frfsBooking -> frfsBooking.status `elem` [DFRFSTicketBooking.FAILED, DFRFSTicketBooking.CANCELLED]) allJourneyFrfsBookings
      nonRefundInitiatedBookings =
        filter
          ( \bkg ->
              ( any
                  ( \paymentBooking ->
                      paymentBooking.frfsTicketBookingId == bkg.id
                        && paymentBooking.status == DFRFSTicketBookingPayment.REFUND_PENDING
                  )
                  paymentBookings
              )
          )
          terminalBookings
      allFailed = not (null terminalBookings) && all (\frfsBooking -> frfsBooking.status == DFRFSTicketBooking.FAILED) allJourneyFrfsBookings
  whenJust (listToMaybe nonRefundInitiatedBookings) $ \_ -> do
    logInfo $ "payment status api markAllRefundBookings: " <> show nonRefundInitiatedBookings
    logInfo $ "allFailed flag in markAllRefundBookings: " <> show allFailed
    person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    payments <- mapM (QFRFSTicketBookingPayment.findNewTBPByBookingId . (.id)) nonRefundInitiatedBookings
    orderShortId <- case listToMaybe (catMaybes payments) of
      Just payment -> do
        order <- QPaymentOrder.findById payment.paymentOrderId >>= fromMaybeM (PaymentOrderNotFound payment.paymentOrderId.getId)
        pure order.shortId.getShortId
      Nothing -> throwError (InvalidRequest "orderShortId not found in markAllRefundBookings")
    frfsConfig <-
      CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId []
        >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show person.merchantOperatingCityId)
    (vendorSplitDetails, amountUpdated) <- SMMFRFS.createVendorSplitFromBookings nonRefundInitiatedBookings person.merchantId person.merchantOperatingCityId Payment.FRFSMultiModalBooking frfsConfig.isFRFSTestingEnabled
    isSplitEnabled <- Payment.getIsSplitEnabled person.merchantId person.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking
    let splitDetails = Payment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amountUpdated vendorSplitDetails
    let refundSplitDetails = mkRefundSplitDetails nonRefundInitiatedBookings
    refundId <- generateGUID
    let lockKey = "markAllRefundBookings:" <> orderShortId
    Redis.withLockRedis lockKey 5 $ do
      when allFailed $ whenJust mbJourneyId $ \journeyId -> QJourney.updateStatus DJourney.FAILED journeyId
      let refundReq =
            Payment.AutoRefundReq
              { orderId = orderShortId,
                requestId = refundId,
                amount = amountUpdated,
                splitSettlementDetails = splitDetails
              }
          createRefundCall refundReq' = Payment.refundOrder person.merchantId person.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion refundReq'
      result <- try @_ @SomeException $ DPayment.refundService (refundReq, Kernel.Types.Id.Id {Kernel.Types.Id.getId = refundId}) (Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant person.merchantId) (Just refundSplitDetails) createRefundCall
      case result of
        Left err -> logError $ "Refund service failed for journey " <> refundId <> ": " <> show err
        Right _ -> do
          logInfo $ "Refund service completed successfully for journey " <> refundId
          forM_ nonRefundInitiatedBookings $ \frfsBooking -> do
            void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_INITIATED frfsBooking.id

          let scheduleAfter = secondsToNominalDiffTime (24 * 60 * 60) -- Schedule for 24 hours later
              jobData =
                JobScheduler.CheckRefundStatusJobData
                  { JobScheduler.refundId = refundId,
                    JobScheduler.numberOfRetries = 0
                  }
          createJobIn @_ @'CheckRefundStatus (Just person.merchantId) (Just person.merchantOperatingCityId) scheduleAfter (jobData :: JobScheduler.CheckRefundStatusJobData)
          logInfo $ "Scheduled refund status check job for " <> refundId <> " in 24 hours (initial check)"

          logInfo $ "payment status api markAllRefundBookings completed"
      pure ()
  where
    mkRefundSplitDetails :: [DFRFSTicketBooking.FRFSTicketBooking] -> [Refunds.Split]
    mkRefundSplitDetails bookings =
      map
        ( \bkg ->
            Refunds.Split
              { splitAmount = bkg.price.amount,
                frfsBookingId = bkg.id.getId
              }
        )
        bookings

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
  m (Maybe DOrder.PaymentOrder)
createPaymentOrder bookings merchantOperatingCityId merchantId amount person paymentType vendorSplitArr = do
  logInfo $ "createPayments vendorSplitArr" <> show vendorSplitArr
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  personEmail <- mapM decrypt person.email
  (orderId, orderShortId) <- getPaymentIds
  ticketBookingPayments' <- processPayments orderId `mapM` bookings
  QFRFSTicketBookingPayment.createMany ticketBookingPayments'
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId merchantOperatingCityId Nothing paymentType
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = orderId.getId,
            orderShortId = orderShortId,
            amount = amount,
            customerId = person.id.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
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
            splitSettlementDetails = Payment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amount vendorSplitArr
          }
  let mocId = merchantOperatingCityId
      commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
      commonMerchantOperatingCityId = Kernel.Types.Id.cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
      createOrderCall = Payment.createOrder merchantId mocId Nothing paymentType (Just person.id.getId) person.clientSdkVersion
  orderResp <- DPayment.createOrderService commonMerchantId (Just $ cast mocId) commonPersonId createOrderReq createOrderCall
  mapM (DPayment.buildPaymentOrder commonMerchantId (Just commonMerchantOperatingCityId) commonPersonId createOrderReq) orderResp
  where
    getPaymentIds = do
      orderShortId <- generateShortId
      orderId <- generateGUID
      isMetroTestTransaction <- asks (.isMetroTestTransaction)
      let updatedOrderShortId = bool (orderShortId.getShortId) ("test-" <> orderShortId.getShortId) isMetroTestTransaction
      return (orderId, updatedOrderShortId)

    processPayments ::
      ( EsqDBReplicaFlow m r,
        BeamFlow m r,
        EncFlow m r,
        ServiceFlow m r
      ) =>
      Id PaymentOrder.PaymentOrder ->
      FTBooking.FRFSTicketBooking ->
      m DFRFSTicketBookingPayment.FRFSTicketBookingPayment
    processPayments orderId booking = do
      ticketBookingPaymentId <- generateGUID
      now <- getCurrentTime
      let ticketBookingPayment =
            DFRFSTicketBookingPayment.FRFSTicketBookingPayment
              { frfsTicketBookingId = booking.id,
                id = ticketBookingPaymentId,
                status = DFRFSTicketBookingPayment.PENDING,
                merchantId = Just booking.merchantId,
                merchantOperatingCityId = Just booking.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now,
                paymentOrderId = orderId
              }
      return ticketBookingPayment
