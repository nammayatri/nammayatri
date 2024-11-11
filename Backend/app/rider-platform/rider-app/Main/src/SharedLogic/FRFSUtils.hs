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
import Data.Aeson as A
import Data.List (groupBy, nub)
import Data.Text (breakOn, drop)
import Domain.Types.AadhaarVerification as DAadhaarVerification
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.Station as Station
import EulerHS.Prelude ((+||), (||+))
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.External.MultiModal.OpenTripPlanner.Types as MultiModal
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (drop)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopMapping as QRSM
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.DynamicLogic
import Tools.Error
import Tools.MultiModal as TMultiModal

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- mbPOrgId `forM` \pOrgId -> B.runInReplica $ CQPOS.findByStationIdAndPOrgId id pOrgId >>= fromMaybeM (PartnerOrgStationNotFoundForStationId pOrgId.getId id.getId)
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

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId stationAPI = do
  station <- B.runInReplica $ CQS.findByStationCode stationAPI.code >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "")
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
    (allLogics, _) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_DISCOUNTS localTime Nothing
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
    travelTime :: Maybe Seconds
  }
  deriving stock (Show)

data TransitRouteInfo = WalkLegInfo WalkInfo | RouteStopLegInfo RouteStopInfo
  deriving stock (Show)

data WalkInfo = WalkInfo
  { travelTime :: Maybe Seconds,
    travelDistance :: Distance
  }
  deriving stock (Show)

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode = do
  routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStationCode
  let routeCodes = nub $ map (.routeCode) routesWithStop
  routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
      possibleRoutes =
        nub $
          catMaybes $
            map
              ( \stops ->
                  let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                   in find
                        ( \stop ->
                            maybe
                              False
                              (\startStopSequence -> stop.stopCode == endStationCode && stop.sequenceNum > startStopSequence)
                              mbStartStopSequence
                        )
                        stops
                        <&> ( \stop -> do
                                case mbStartStopSequence of
                                  Just startStopSequence ->
                                    let totalStops = stop.sequenceNum - startStopSequence
                                        totalTravelTime =
                                          foldr
                                            ( \stop' acc ->
                                                if stop'.sequenceNum > startStopSequence && stop'.sequenceNum <= stop.sequenceNum
                                                  then case (acc, stop'.estimatedTravelTimeFromPreviousStop) of
                                                    (Just acc', Just travelTime) -> Just (acc' + travelTime)
                                                    _ -> Nothing
                                                  else acc
                                            )
                                            (Just $ Seconds 0)
                                            stops
                                     in (stop.routeCode, Just totalStops, totalTravelTime)
                                  Nothing -> (stop.routeCode, Nothing, Nothing)
                            )
              )
              groupedStops
  routes <- QRoute.findByRouteCodes (map (\(routeCode, _, _) -> routeCode) possibleRoutes)
  return $
    map
      ( \route ->
          let routeData = find (\(routeCode, _, _) -> routeCode == route.code) possibleRoutes
           in RouteStopInfo
                { route,
                  totalStops = (\(_, totalStops, _) -> totalStops) =<< routeData,
                  startStopCode = startStationCode,
                  endStopCode = endStationCode,
                  travelTime = (\(_, _, travelTime) -> travelTime) =<< routeData
                }
      )
      routes

-- TODO :: This to be handled from OTP, Currently Hardcode for Chennai
getPossibleTransitRoutesBetweenTwoStops :: (MonadFlow m, ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> m [[TransitRouteInfo]]
getPossibleTransitRoutesBetweenTwoStops merchantId merchantOperatingCityId startStationCode endStationCode = do
  case (startStationCode, endStationCode) of
    ("MBTcSIip", "jQaLNViL") -> do
      routes <- QRoute.findByRouteCodes ["jylLjHej", "BTuKbmBy"]
      return $
        [ map
            ( \route ->
                if route.code == "jylLjHej"
                  then
                    RouteStopLegInfo $
                      RouteStopInfo
                        { route,
                          totalStops = Just 6,
                          startStopCode = "MBTcSIip",
                          endStopCode = "TiulEaYs",
                          travelTime = Just $ Seconds 660
                        }
                  else
                    RouteStopLegInfo $
                      RouteStopInfo
                        { route,
                          totalStops = Just 8,
                          startStopCode = "TiulEaYs",
                          endStopCode = "jQaLNViL",
                          travelTime = Just $ Seconds 1440
                        }
            )
            routes
        ]
    _ -> do
      resp <- try @_ @SomeException (getTransitRoutesForStops merchantId merchantOperatingCityId startStationCode endStationCode)
      case resp of
        Left _ -> do
          logDebug $ "transit routes not available for startStopCode: " <> startStationCode <> " to endStopCode: " <> endStationCode
          return $ []
        Right transitRoute -> return transitRoute

getTransitRoutesForStops :: (MonadFlow m, ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> m [[TransitRouteInfo]]
getTransitRoutesForStops merchantId merchantOperatingCityId startStationCode endStationCode = do
  now <- getCurrentTime
  startStation <- QStation.findByStationCode startStationCode >>= fromMaybeM (InternalError "startStation not found")
  endStation <- QStation.findByStationCode endStationCode >>= fromMaybeM (InternalError "endStation not found")
  startLat <- startStation.lat & fromMaybeM (InternalError "startStationLat not found")
  startLon <- startStation.lon & fromMaybeM (InternalError "startStationLon not found")
  endLat <- endStation.lat & fromMaybeM (InternalError "endStationLat not found")
  endLon <- endStation.lon & fromMaybeM (InternalError "endStationLon not found")

  let transitRoutesReq =
        GetTransitRoutesReq
          { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = startLat, longitude = startLon}}},
            destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = endLat, longitude = endLon}}},
            arrivalTime = Nothing,
            departureTime = Just now,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Just [Just MultiModal.TransportMode {mode = "BUS"}]
          }

  transitServiceReq <- TMultiModal.getTransitServiceReq merchantId merchantOperatingCityId

  allRoutes :: MultiModal.MultiModalResponse <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")

  mapM
    ( \transitRoute -> do
        transitRoutes <-
          mapM
            ( \transitLeg -> do
                case transitLeg.mode of
                  MultiModal.Bus -> do
                    startStopDetails <- transitLeg.fromStopDetails & fromMaybeM (InternalError $ "fromStopDetails not found for " <> show transitLeg)
                    endStopDetails <- transitLeg.toStopDetails & fromMaybeM (InternalError $ "toStopDetails not found for " <> show transitLeg)
                    startStopCodeFromOtp <- startStopDetails.gtfsId & fromMaybeM (InternalError $ "start stopCode not found for " <> show startStopDetails)
                    endStopCodeFromOtp <- endStopDetails.gtfsId & fromMaybeM (InternalError $ "end stopCode not found for " <> show endStopDetails)
                    routeDetails <- transitLeg.routeDetails & fromMaybeM (InternalError $ "routeDetails not found for " <> show transitLeg)
                    routeCodeFromOtp <- routeDetails.gtfsId & fromMaybeM (InternalError $ "route shortName not found for " <> show routeDetails)
                    let startStopCode = cutAfterColon startStopCodeFromOtp
                        endStopCode = cutAfterColon endStopCodeFromOtp
                        routeCode = cutAfterColon routeCodeFromOtp
                    routeMapping <- QRoute.findByRouteCode routeCode
                    startStopMapping <- QRSM.findByRouteCodeAndStopCode routeCode startStopCode
                    endStopMapping <- QRSM.findByRouteCodeAndStopCode routeCode endStopCode
                    case (routeMapping, startStopMapping, endStopMapping) of
                      (Just route, Just startStop, Just endStop) ->
                        return $
                          RouteStopLegInfo $
                            RouteStopInfo
                              { route,
                                totalStops = Just (endStop.sequenceNum - startStop.sequenceNum),
                                startStopCode,
                                endStopCode,
                                travelTime = Just transitLeg.duration
                              }
                      (_, _, _) ->
                        return $
                          WalkLegInfo $
                            WalkInfo
                              { travelTime = Just transitLeg.duration,
                                travelDistance = transitLeg.distance
                              }
                  _ ->
                    return $
                      WalkLegInfo $
                        WalkInfo
                          { travelTime = Just transitLeg.duration,
                            travelDistance = transitLeg.distance
                          }
            )
            transitRoute.legs
        let busLegsCount = length $ filter isRouteStopLegInfo transitRoutes
        if busLegsCount > 1 then return transitRoutes else return []
    )
    allRoutes.routes
  where
    cutAfterColon :: Text -> Text
    cutAfterColon t =
      case breakOn ":" t of
        (_, "") -> t
        (_, after) -> drop 1 after
    isRouteStopLegInfo :: TransitRouteInfo -> Bool
    isRouteStopLegInfo (RouteStopLegInfo _) = True
    isRouteStopLegInfo _ = False
