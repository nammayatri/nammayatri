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
import Data.List (groupBy, nub, sortBy)
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
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Domain.Types.Station as Station
import EulerHS.Prelude ((+||), (||+))
import Kernel.Beam.Functions as B
import Kernel.Prelude
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
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Tools.DynamicLogic
import Tools.Error

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

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> Id DMOC.MerchantOperatingCity -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId merchantOperatingCityId stationAPI = do
  station <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId stationAPI.code merchantOperatingCityId >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "and merchantOperatingCityId: " +|| merchantOperatingCityId ||+ "")
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
    stops :: Maybe [RouteStopMapping.RouteStopMapping],
    travelTime :: Maybe Seconds
  }

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
  routes <- QRoute.findByRouteCodes (map (\(routeCode, _, _, _) -> routeCode) possibleRoutes)
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

-- TODO :: This to be handled from OTP, Currently Hardcode for Chennai
getPossibleTransitRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [[RouteStopInfo]]
getPossibleTransitRoutesBetweenTwoStops startStationCode endStationCode = do
  case (startStationCode, endStationCode) of
    ("MBTcSIip", "jQaLNViL") -> do
      routes <- QRoute.findByRouteCodes ["jylLjHej", "BTuKbmBy"]
      return $
        [ map
            ( \route ->
                if route.code == "jylLjHej"
                  then
                    RouteStopInfo
                      { route,
                        totalStops = Just 6,
                        stops = Nothing,
                        startStopCode = "MBTcSIip",
                        endStopCode = "TiulEaYs",
                        travelTime = Just $ Seconds 660
                      }
                  else
                    RouteStopInfo
                      { route,
                        totalStops = Just 8,
                        stops = Nothing,
                        startStopCode = "TiulEaYs",
                        endStopCode = "jQaLNViL",
                        travelTime = Just $ Seconds 1440
                      }
            )
            routes
        ]
    _ -> return []
