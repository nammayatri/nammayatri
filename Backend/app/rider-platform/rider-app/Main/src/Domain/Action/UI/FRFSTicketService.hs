module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import BecknV2.FRFS.Enums hiding (END, START)
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import Data.List (groupBy, nub, nubBy)
import qualified Data.List.NonEmpty as NonEmpty hiding (groupBy, map, nub, nubBy)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.Common as Common
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified Environment
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, fromList, groupBy, id, length, map, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Maps.Types
import Kernel.External.MultiModal.Utils
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude hiding (whenJust)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.JourneyLeg.Types as JLT
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import SharedLogic.FRFSUtils
import qualified SharedLogic.FRFSUtils as Utils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Payment as Payment

getFrfsRoutes ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Maybe Text ->
  Maybe Text ->
  Context.City ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
getFrfsRoutes (_personId, _mId) mbEndStationCode mbStartStationCode _city _vehicleType = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity _mId _city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _mId.getId <> "-city-" <> show _city)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory _vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory _vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  case (mbStartStationCode, mbEndStationCode) of
    (Just startStationCode, Just endStationCode) -> do
      routesInfo <- getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig
      return $
        map
          ( \routeInfo ->
              let stops =
                    ( mapWithIndex
                        ( \idx stop ->
                            FRFSStationAPI
                              { name = stop.stopName,
                                code = stop.stopName,
                                lat = Just stop.stopPoint.lat,
                                lon = Just stop.stopPoint.lon,
                                stationType = Just (if idx == 0 then START else if maybe False (\stops' -> idx < length stops') routeInfo.stops then INTERMEDIATE else END),
                                sequenceNum = Just stop.sequenceNum,
                                address = Nothing,
                                distance = Nothing,
                                color = Nothing,
                                towards = Nothing
                              }
                        )
                    )
                      <$> routeInfo.stops
               in FRFSTicketService.FRFSRouteAPI
                    { code = routeInfo.route.code,
                      shortName = routeInfo.route.shortName,
                      longName = routeInfo.route.longName,
                      startPoint = routeInfo.route.startPoint,
                      endPoint = routeInfo.route.endPoint,
                      totalStops = routeInfo.totalStops,
                      stops = stops,
                      timeBounds = Just routeInfo.route.timeBounds,
                      waypoints = Nothing
                    }
          )
          routesInfo
    _ -> do
      routes <- B.runInReplica $ QRoute.findAllByVehicleType Nothing Nothing _vehicleType integratedBPPConfig.id
      return $
        map
          ( \Route.Route {..} -> FRFSTicketService.FRFSRouteAPI {totalStops = Nothing, stops = Nothing, waypoints = Nothing, timeBounds = Nothing, ..}
          )
          routes
  where
    mapWithIndex f xs = zipWith f [0 ..] xs

data StationResult = StationResult
  { code :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    lat :: Double,
    lon :: Double,
    stationType :: Maybe StationType,
    sequenceNum :: Maybe Int
  }
  deriving (Show)

instance HasCoordinates StationResult where
  getCoordinates stop = LatLong (stop.lat) (stop.lon)

getFrfsRoute ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Text ->
  Context.City ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  Environment.Flow API.Types.UI.FRFSTicketService.FRFSRouteAPI
getFrfsRoute (_personId, _mId) routeCode _mbCity _vehicleType = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity _mId _mbCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _mId.getId <> "-city-" <> show _mbCity)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory _vehicleType) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory _vehicleType ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  route <- QRoute.findByRouteCode routeCode integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode)
  routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
  return $
    FRFSTicketService.FRFSRouteAPI
      { code = route.code,
        shortName = route.shortName,
        longName = route.longName,
        startPoint = route.startPoint,
        endPoint = route.endPoint,
        totalStops = Just $ length stopsSortedBySequenceNumber,
        stops =
          Just $
            map
              ( \stop ->
                  FRFSStationAPI
                    { name = stop.stopName,
                      code = stop.stopName,
                      lat = Just stop.stopPoint.lat,
                      lon = Just stop.stopPoint.lon,
                      stationType = Nothing,
                      sequenceNum = Just stop.sequenceNum,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      towards = Nothing
                    }
              )
              stopsSortedBySequenceNumber,
        timeBounds = Just route.timeBounds,
        waypoints = route.polyline <&> decode <&> fmap (\point -> LatLong {lat = point.latitude, lon = point.longitude})
      }

getFrfsStations ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Context.City ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Text ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations (_personId, mId) mbCity mbEndStationCode mbOrigin mbRouteCode mbStartStationCode vehicleType_ = do
  merchantOpCity <-
    case mbCity of
      Nothing ->
        CQMOC.findById (Id "407c445a-2200-c45f-8d67-6f6dbfa28e73")
          >>= fromMaybeM (MerchantOperatingCityNotFound "merchantOpCityId-407c445a-2200-c45f-8d67-6f6dbfa28e73")
      Just city ->
        CQMOC.findByMerchantIdAndCity mId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType_ ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  case (mbRouteCode, mbStartStationCode, mbEndStationCode) of
    -- Return possible Start stops, when End Stop is Known
    (Nothing, Nothing, Just endStationCode) -> do
      currentTime <- getCurrentTime
      routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode endStationCode integratedBPPConfig.id
      let routeCodes = nub $ map (.routeCode) routesWithStop
      let integratedBPPConfigIds = replicate (length routeCodes) (integratedBPPConfig.id)
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes integratedBPPConfigIds
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
          groupedStopsByRouteCode = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
          possibleStartStops =
            nubBy (\a b -> a.stopCode == b.stopCode) $
              concatMap
                ( \stops ->
                    let mbEndStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == endStationCode) stops
                     in sortBy (compare `on` (.sequenceNum)) $ filter (\stop -> maybe False (\endStopSequence -> stop.stopCode /= endStationCode && stop.sequenceNum < endStopSequence) mbEndStopSequence) stops
                )
                groupedStopsByRouteCode
      let startStops =
            map
              ( \routeStop ->
                  FRFSStationAPI
                    { name = routeStop.stopName,
                      code = routeStop.stopCode,
                      lat = Just routeStop.stopPoint.lat,
                      lon = Just routeStop.stopPoint.lon,
                      stationType = Nothing,
                      sequenceNum = Nothing,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      towards = Nothing
                    }
              )
              possibleStartStops
      mkStationsAPIWithDistance merchantOpCity startStops mbOrigin
    -- Return possible End stops, when Route & Start Stop is Known
    (Just routeCode, Just startStationCode, Nothing) -> do
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id
      currentTime <- getCurrentTime
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
          startSeqNum = fromMaybe 0 ((.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) serviceableStops)
          filteredRouteStops = filter (\stop -> stop.stopCode /= startStationCode && stop.sequenceNum > startSeqNum) serviceableStops
      let endStops =
            map
              ( \routeStop ->
                  FRFSStationAPI
                    { name = routeStop.stopName,
                      code = routeStop.stopCode,
                      lat = Just routeStop.stopPoint.lat,
                      lon = Just routeStop.stopPoint.lon,
                      stationType = Just (if routeStop.sequenceNum == 1 then START else if routeStop.sequenceNum < length filteredRouteStops then INTERMEDIATE else END),
                      sequenceNum = Just routeStop.sequenceNum,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      towards = Nothing
                    }
              )
              filteredRouteStops
      mkStationsAPIWithDistance merchantOpCity endStops mbOrigin
    -- Return all Stops, when only the Route is Known
    (Just routeCode, Nothing, Nothing) -> do
      currentTime <- getCurrentTime
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
          stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
      let stops =
            map
              ( \routeStop ->
                  FRFSStationAPI
                    { name = routeStop.stopName,
                      code = routeStop.stopCode,
                      lat = Just routeStop.stopPoint.lat,
                      lon = Just routeStop.stopPoint.lon,
                      stationType = Just (if routeStop.sequenceNum == 1 then START else if routeStop.sequenceNum < length stopsSortedBySequenceNumber then INTERMEDIATE else END),
                      sequenceNum = Just routeStop.sequenceNum,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      towards = Nothing
                    }
              )
              stopsSortedBySequenceNumber
      mkStationsAPIWithDistance merchantOpCity stops mbOrigin
    -- Return all possible End Stops across all the Routes, when only the Start Stop is Known
    (Nothing, Just startStationCode, Nothing) -> do
      currentTime <- getCurrentTime
      routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStationCode integratedBPPConfig.id
      let routeCodes = nub $ map (.routeCode) routesWithStop
      let integratedBPPConfigIds' = replicate (length routeCodes) (integratedBPPConfig.id)
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes integratedBPPConfigIds'
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
          groupedStopsByRouteCode = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
          possibleEndStops =
            nubBy (\a b -> a.stopCode == b.stopCode) $
              concatMap
                ( \stops ->
                    let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                     in sortBy (compare `on` (.sequenceNum)) $ filter (\stop -> maybe False (\startStopSequence -> stop.stopCode /= startStationCode && stop.sequenceNum > startStopSequence) mbStartStopSequence) stops
                )
                groupedStopsByRouteCode
      let endStops =
            map
              ( \routeStop ->
                  FRFSStationAPI
                    { name = routeStop.stopName,
                      code = routeStop.stopCode,
                      lat = Just routeStop.stopPoint.lat,
                      lon = Just routeStop.stopPoint.lon,
                      stationType = Nothing,
                      sequenceNum = Nothing,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      towards = Nothing
                    }
              )
              possibleEndStops
      mkStationsAPIWithDistance merchantOpCity endStops mbOrigin
    -- Return all the Stops
    _ -> do
      stations <- B.runInReplica $ QStation.findAllByVehicleType Nothing Nothing vehicleType_ integratedBPPConfig.id
      let stops =
            map
              ( \Station {..} ->
                  FRFSStationAPI
                    { color = Nothing,
                      distance = Nothing,
                      sequenceNum = Nothing,
                      stationType = Nothing,
                      towards = Nothing,
                      ..
                    }
              )
              stations
      mkStationsAPIWithDistance merchantOpCity stops mbOrigin
  where
    mkStationsAPIWithDistance merchantOpCity stations = \case
      Just origin -> tryStationsAPIWithOSRMDistances mId merchantOpCity origin stations
      Nothing -> return stations

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Context.City -> Spec.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (mbPersonId, merchantId) mbCity vehicleType_ req = do
  let frfsRouteDetails =
        [ FRFSRouteDetails
            { routeCode = req.routeCode,
              startStationCode = req.fromStationCode,
              endStationCode = req.toStationCode
            }
        ]
  postFrfsSearchHandler (mbPersonId, merchantId) mbCity vehicleType_ req frfsRouteDetails Nothing Nothing [] DIBC.APPLICATION

postFrfsSearchHandler ::
  CallExternalBPP.FRFSSearchFlow m r =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Kernel.Prelude.Maybe Context.City ->
  Spec.VehicleCategory ->
  API.Types.UI.FRFSTicketService.FRFSSearchAPIReq ->
  [FRFSRouteDetails] ->
  Maybe (Id DPO.PartnerOrgTransaction) ->
  Maybe (Id DPO.PartnerOrganization) ->
  [JLT.MultiModalJourneyRouteDetails] ->
  DIBC.PlatformType ->
  m API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearchHandler (mbPersonId, merchantId) mbCity vehicleType_ FRFSSearchAPIReq {..} frfsRouteDetails mbPOrgTxnId mbPOrgId mbJourneyRouteDetails platformType = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchantOperatingCityId <-
    case mbCity of
      Just city ->
        CQMOC.findByMerchantIdAndCity merchantId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
          >>= return . (.id)
      Nothing ->
        CQP.findCityInfoById personId
          >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
          >>= return . (.merchantOperatingCityId)

  merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOperatingCityId.getId)
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType_ ||+ "Platform Type:" +|| platformType ||+ "")
  (fromStation, toStation) <- do
    fromStationInfo <- QStation.findByStationCode fromStationCode integratedBPPConfig.id >>= fromMaybeM (InvalidRequest $ "Invalid from station id: " <> fromStationCode <> " or integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
    toStationInfo <- QStation.findByStationCode toStationCode integratedBPPConfig.id >>= fromMaybeM (InvalidRequest $ "Invalid to station id: " <> toStationCode <> " or integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
    return (fromStationInfo, toStationInfo)
  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- QRoute.findByRouteCode routeCode' integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      routeCode

  searchReqId <- generateGUID
  now <- getCurrentTime
  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = vehicleType_,
            merchantId = merchantId,
            merchantOperatingCityId = fromStation.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            fromStationId = fromStation.id,
            toStationId = toStation.id,
            routeId = route <&> (.id),
            riderId = personId,
            partnerOrgTransactionId = mbPOrgTxnId,
            partnerOrgId = mbPOrgId,
            journeyLegInfo = journeySearchData,
            journeyLegStatus = Just JLT.InPlan,
            journeyRouteDetails = mbJourneyRouteDetails,
            isOnSearchReceived = Nothing,
            ..
          }
  QFRFSSearch.create searchReq
  CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq frfsRouteDetails integratedBPPConfig
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote (mbPersonId, _) searchId_ = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  search <- QFRFSSearch.findById searchId_ >>= fromMaybeM (InvalidRequest "Invalid search id")
  unless (personId == search.riderId) $ throwError AccessDenied
  (quotes :: [DFRFSQuote.FRFSQuote]) <- B.runInReplica $ QFRFSQuote.findAllBySearchId searchId_

  mapM
    ( \quote -> do
        (stations :: [FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
        let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
            discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
        return $
          FRFSTicketService.FRFSQuoteAPIRes
            { quoteId = quote.id,
              _type = quote._type,
              price = quote.price.amount,
              priceWithCurrency = mkPriceAPIEntity quote.price,
              quantity = quote.quantity,
              validTill = quote.validTill,
              vehicleType = quote.vehicleType,
              discountedTickets = quote.discountedTickets,
              eventDiscountAmount = quote.eventDiscountAmount,
              ..
            }
    )
    quotes

postFrfsQuoteV2Confirm :: CallExternalBPP.FRFSConfirmFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId req = do
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  (rider, dConfirmRes) <- confirm
  -- handle (errHandler dConfirmRes.booking) $
  --   void $ withShortRetry $ CallBPP.init dConfirmRes.bppSubscriberUrl becknInitReq
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< dConfirmRes.routeStationsJson
      discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< dConfirmRes.discountsJson
  now <- getCurrentTime
  when (dConfirmRes.status == DFRFSTicketBooking.NEW && dConfirmRes.validTill > now) $ do
    bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes' DIBC.APPLICATION
  return $ makeBookingStatusAPI dConfirmRes discounts routeStations stations merchantOperatingCity.city
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc
    confirm :: CallExternalBPP.FRFSConfirmFlow m r => m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    confirm = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
      unless (personId == quote.riderId) $ throwError AccessDenied
      let discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
      selectedDiscounts <- validateDiscounts req.discounts (fromMaybe [] discounts)

      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ InvalidRequest "Quote expired"
      maybeM (buildAndCreateBooking rider quote selectedDiscounts) (\booking -> return (rider, booking)) (QFRFSTicketBooking.findByQuoteId quoteId)

    validateDiscounts :: (MonadFlow m) => [FRFSDiscountReq] -> [FRFSDiscountRes] -> m [FRFSDiscountRes]
    validateDiscounts selectedDiscounts allDiscounts = do
      let selecetedDiscountCodes = map (.code) selectedDiscounts
          eligibleDiscountCodes = map (.code) $ filter (.eligibility) allDiscounts
      unless (all (flip elem eligibleDiscountCodes) selecetedDiscountCodes) $ throwError DiscountsIneligible
      return $ intersectBy (\discount selectedDiscount -> discount.code == selectedDiscount.code) allDiscounts selectedDiscounts
      where
        intersectBy :: (a -> b -> Bool) -> [a] -> [b] -> [a]
        intersectBy _ [] _ = []
        intersectBy _ _ [] = []
        intersectBy f as bs = filter (\a -> any (f a) bs) as

    buildAndCreateBooking :: CallExternalBPP.FRFSConfirmFlow m r => Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> [FRFSDiscountRes] -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    buildAndCreateBooking rider quote@DFRFSQuote.FRFSQuote {..} selectedDiscounts = do
      uuid <- generateGUID
      now <- getCurrentTime
      mbSearch <- QFRFSSearch.findById searchId
      let appliedDiscountsJson = encodeToText selectedDiscounts
          totalDiscount =
            foldr
              (\selectedDiscount discountAmount -> discountAmount + selectedDiscount.price.amount)
              (HighPrecMoney 0.0)
              selectedDiscounts
      let discountedPrice = modifyPrice quote.price $ \p -> max (HighPrecMoney 0.0) $ HighPrecMoney ((p.getHighPrecMoney) * (toRational quote.quantity)) - totalDiscount
      let isFareChanged = isJust oldCacheDump
      let journeyRouteDetails' = maybe [] (.journeyRouteDetails) mbSearch
      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote.merchantId,
                price = discountedPrice,
                estimatedPrice = discountedPrice,
                finalPrice = Nothing,
                paymentTxnId = Nothing,
                bppBankAccountNumber = Nothing,
                bppBankCode = Nothing,
                cancellationCharges = Nothing,
                refundAmount = Nothing,
                isBookingCancellable = Nothing,
                customerCancelled = False,
                payerVpa = Nothing,
                cashbackPayoutOrderId = Nothing,
                cashbackStatus = if isJust quote.discountedTickets then Just DFTB.PENDING else Nothing,
                bppDelayedInterest = quote.bppDelayedInterest,
                discountsJson = Just appliedDiscountsJson,
                journeyLegOrder = mbSearch >>= (.journeyLegInfo) <&> (.journeyLegOrder),
                journeyId = Id <$> (mbSearch >>= (.journeyLegInfo) <&> (.journeyId)),
                journeyOnInitDone = Nothing,
                journeyLegStatus = mbSearch >>= (.journeyLegStatus),
                journeyRouteDetails = journeyRouteDetails',
                startTime = Just now, -- TODO
                isFareChanged = Just isFareChanged,
                googleWalletJWTUrl = Nothing,
                isDeleted = Just False,
                isSkipped = Just False,
                ..
              }
      QFRFSTicketBooking.create booking
      return (rider, booking)

    makeBookingStatusAPI booking discounts routeStations stations city =
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          price = booking.price.amount,
          priceWithCurrency = mkPriceAPIEntity booking.price,
          quantity = booking.quantity,
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          discountedTickets = booking.discountedTickets,
          eventDiscountAmount = booking.eventDiscountAmount,
          isFareChanged = booking.isFareChanged,
          googleWalletJWTUrl = booking.googleWalletJWTUrl,
          ..
        }

postFrfsQuoteConfirm :: CallExternalBPP.FRFSConfirmFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId = postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId (API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq {discounts = []})

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

webhookHandlerFRFSTicket :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow ()
webhookHandlerFRFSTicket paymentOrderId merchantId = do
  logDebug $ "frfs ticket order bap webhookc call" <> paymentOrderId.getShortId
  order <- QPaymentOrder.findByShortId paymentOrderId >>= fromMaybeM (PaymentOrderNotFound paymentOrderId.getShortId)
  bookingByOrderId <- QFRFSTicketBookingPayment.findByPaymentOrderId order.id >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingByOrderId.frfsTicketBookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  void $ frfsBookingStatus (booking.riderId, merchantId) booking

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  frfsBookingStatus (personId, merchantId_) booking

frfsBookingStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
frfsBookingStatus (personId, merchantId_) booking' = do
  let bookingId = booking'.id
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking'.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.status /= DFRFSTicketBooking.CANCELLED && booking'.validTill < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking booking
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.FAILED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking)
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      when (paymentBookingStatus == FRFSTicketService.FAILURE) do
        void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
        let mPrice = Common.mkPrice (Just booking'.price.currency) (HighPrecMoney $ toRational (0 :: Int))
        void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById mPrice mPrice booking.id
      when (paymentBookingStatus == FRFSTicketService.SUCCESS) do
        void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
      let paymentStatusAPI =
            case paymentBookingStatus of
              FRFSTicketService.FAILURE -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
              FRFSTicketService.SUCCESS -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_PENDING
              _ -> Nothing
      let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.CONFIRMING -> do
      if booking.validTill < now
        then do
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING bookingId
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else do
          buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      void $ CallExternalBPP.status merchant.id merchantOperatingCity bapConfig booking DIBC.APPLICATION
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking)
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      if paymentBookingStatus == FRFSTicketService.FAILURE
        then do
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
            else do
              txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
              let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing Nothing
              paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
              let paymentObj =
                    Just $
                      FRFSTicketService.FRFSBookingPaymentAPI
                        { status = paymentStatus_,
                          paymentOrder = paymentOrder_,
                          transactionId = Nothing
                        }
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking)
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      if paymentBookingStatus == FRFSTicketService.FAILURE
        then do
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
            else
              if paymentBookingStatus == FRFSTicketService.SUCCESS
                then do
                  -- Add default TTL of 1 min or the value provided in the config
                  let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
                  transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
                  txnId <- getSuccessTransactionId transactions
                  void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.SUCCESS booking.id
                  void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id
                  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL) (Just txnId.getId)
                  let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                  mRiderNumber <- mapM decrypt person.mobileNumber
                  void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent paymentStatusResp.payerVpa bookingId
                  void $ CallExternalBPP.confirm processOnConfirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) updatedBooking DIBC.APPLICATION
                  buildFRFSTicketBookingStatusAPIRes updatedBooking paymentSuccess
                else do
                  paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
                  txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                  let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                      paymentObj =
                        Just
                          FRFSTicketService.FRFSBookingPaymentAPI
                            { status = paymentStatus_,
                              paymentOrder = paymentOrder_,
                              transactionId = Nothing
                            }
                  buildFRFSTicketBookingStatusAPIRes booking paymentObj
    DFRFSTicketBooking.CANCELLED -> do
      updateTotalOrderValueAndSettlementAmount booking bapConfig
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      let mbPaymentObj = paymentBooking <&> \tbp -> FRFSTicketService.FRFSBookingPaymentAPI {status = Utils.mkTBPStatusAPI tbp.status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.COUNTER_CANCELLED -> do
      updateTotalOrderValueAndSettlementAmount booking bapConfig
      buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.CANCEL_INITIATED -> do
      buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.TECHNICAL_CANCEL_REJECTED -> do
      buildFRFSTicketBookingStatusAPIRes booking Nothing
  where
    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    processOnConfirm onConfirmReq = do
      (merchant', booking'') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking'' onConfirmReq

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCityId booking = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
      isSplitEnabled_ <- Payment.getIsSplitEnabled merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = paymentOrder.id.getId,
                orderShortId = paymentOrder.shortId.getShortId,
                amount = paymentOrder.amount,
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
                splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled_ paymentOrder.amount []
              }
      DPayment.createOrderService commonMerchantId (Just $ cast merchantOperatingCityId) commonPersonId createOrderReq (createOrderCall merchantOperatingCityId booking)

    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.SUBWAY -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking

    createOrderCall merchantOperatingCityId booking = Payment.createOrder merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
    orderStatusCall merchantOperatingCityId booking = Payment.orderStatus merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
    commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId_

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL transactionId =
      let validTill' = mTTL & fromMaybe validTill
          newPaymentTxnId = transactionId <|> paymentTxnId
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', paymentTxnId = newPaymentTxnId, ..}
    getSuccessTransactionId transactions = do
      let successTransactions = filter (\transaction -> transaction.status == Payment.CHARGED) transactions
      case successTransactions of
        [] -> throwError $ InvalidRequest "No successful transaction found"
        [transaction] -> return transaction.id
        _ -> throwError $ InvalidRequest "Multiple successful transactions found"

updateTotalOrderValueAndSettlementAmount :: DFRFSTicketBooking.FRFSTicketBooking -> BecknConfig -> Environment.Flow ()
updateTotalOrderValueAndSettlementAmount booking bapConfig = do
  paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  let finderFee :: Price = Common.mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      finderFeeForEachTicket = modifyPrice finderFee $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational booking.quantity)
  tOrderPrice <- DACFOC.totalOrderValue paymentBooking.status booking
  let tOrderValue = modifyPrice tOrderPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) / (toRational booking.quantity)
  settlementAmount <- tOrderValue `subtractPrice` finderFeeForEachTicket
  void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById settlementAmount tOrderValue booking.id

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Maybe Spec.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, merchantId) mbLimit mbOffset mbVehicleCategory = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId mbLimit mbOffset personId mbVehicleCategory
  case mbVehicleCategory of
    Just Spec.BUS -> mapM (frfsBookingStatus (personId, merchantId)) bookings
    _ -> mapM (`buildFRFSTicketBookingStatusAPIRes` Nothing) bookings

buildFRFSTicketBookingStatusAPIRes :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe FRFSTicketService.FRFSBookingPaymentAPI -> Environment.Flow FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking payment = do
  stations <- mapM (Utils.mkPOrgStationAPI booking.partnerOrgId booking.merchantOperatingCityId booking.vehicleType) =<< (decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db"))
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
      discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< booking.discountsJson
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking booking
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {..} ->
              FRFSTicketService.FRFSTicketAPI {..}
          )
          tickets'
  return $
    FRFSTicketService.FRFSTicketBookingStatusAPIRes
      { bookingId = booking.id,
        city = merchantOperatingCity.city,
        updatedAt = booking.updatedAt,
        createdAt = booking.createdAt,
        _type = booking._type,
        price = booking.price.amount,
        priceWithCurrency = mkPriceAPIEntity booking.price,
        quantity = booking.quantity,
        validTill = booking.validTill,
        vehicleType = booking.vehicleType,
        status = booking.status,
        discountedTickets = booking.discountedTickets,
        eventDiscountAmount = booking.eventDiscountAmount,
        payment = payment <&> (\p -> p {transactionId = booking.paymentTxnId}),
        isFareChanged = booking.isFareChanged,
        googleWalletJWTUrl = booking.googleWalletJWTUrl,
        ..
      }

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus Payment.NEW = FRFSTicketService.NEW
makeTicketBookingPaymentAPIStatus PENDING_VBV = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus CHARGED = FRFSTicketService.SUCCESS
makeTicketBookingPaymentAPIStatus AUTHENTICATION_FAILED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZATION_FAILED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus JUSPAY_DECLINED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZING = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus COD_INITIATED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus STARTED = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus AUTO_REFUNDED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus CLIENT_AUTH_TOKEN_EXPIRED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus Payment.CANCELLED = FRFSTicketService.FAILURE

cancelFRFSTicketBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow ()
cancelFRFSTicketBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id

postFrfsBookingCanCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCanCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show ticketBooking.merchantOperatingCityId)
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  unless (ticketBooking.status == DFRFSTicketBooking.CONFIRMED) $ throwError (InvalidRequest "Cancellation during incorrect status")
  -- tickets <- QFRFSTicket.findAllByTicketBookingId ticketBooking.id
  -- unless (all (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets) $ throwError (InvalidRequest "Cancellation during incorrect status")
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.SOFT_CANCEL ticketBooking DIBC.APPLICATION
  return APISuccess.Success

getFrfsBookingCanCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
getFrfsBookingCanCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")
  return $
    FRFSCanCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = getAbsoluteValue ticketBooking.refundAmount,
        isCancellable = ticketBooking.isBookingCancellable
      }

postFrfsBookingCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (InvalidRequest $ "Invalid merchant operating city id" <> ticketBooking.merchantOperatingCityId.getId)
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL ticketBooking DIBC.APPLICATION
  return APISuccess.Success

getFrfsBookingCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow FRFSTicketService.FRFSCancelStatus
getFrfsBookingCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  pure
    FRFSTicketService.FRFSCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = if ticketBooking.status == DFRFSTicketBooking.CANCELLED then getAbsoluteValue ticketBooking.refundAmount else Nothing
      }

getAbsoluteValue :: Maybe HighPrecMoney -> Maybe HighPrecMoney
getAbsoluteValue mbRefundAmount = case mbRefundAmount of
  Nothing -> Nothing
  Just rfValue -> do
    let HighPrecMoney value = rfValue
    Just (HighPrecMoney $ abs value)

getFrfsConfig :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Context.City -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSConfigAPIRes
getFrfsConfig (pId, mId) opCity = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity mId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> " ,city: " <> show opCity)
  Domain.Types.FRFSConfig.FRFSConfig {..} <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOpCity.id [] >>= fromMaybeM (InvalidRequest "FRFS Config not found")
  stats <- maybe (pure Nothing) CQP.findPersonStatsById pId
  let isEventOngoing' = fromMaybe False isEventOngoing
      ticketsBookedInEvent = fromMaybe 0 ((.ticketsBookedInEvent) =<< stats)
  return FRFSTicketService.FRFSConfigAPIRes {isEventOngoing = isEventOngoing', ..}

-- TODO :: Filter the Stops which are always the END stop for all the routes as it can never be a possible START or INTERMEDIATE stop.
getFrfsAutocomplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  Context.City ->
  Kernel.External.Maps.Types.LatLong ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  Environment.Flow API.Types.UI.FRFSTicketService.AutocompleteRes
getFrfsAutocomplete (_, mId) mbInput mbLimit mbOffset opCity origin vehicle = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity mId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> " ,city: " <> show opCity)
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOpCity.id []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show merchantOpCity.id)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicle) DIBC.APPLICATION
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicle ||+ "Platform Type:" +|| DIBC.APPLICATION ||+ "")
  case mbInput of
    Nothing -> do
      allStops <- QStation.findAllByVehicleType Nothing Nothing vehicle integratedBPPConfig.id
      currentTime <- getCurrentTime
      let serviceableStops = DTB.findBoundedDomain allStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) allStops
          stopsWithinRadius =
            filter
              ( \stop ->
                  let straightLineDist = highPrecMetersToMeters (CD.distanceBetweenInMeters origin (LatLong (fromMaybe merchantOpCity.lat stop.lat) (fromMaybe merchantOpCity.long stop.lon)))
                   in straightLineDist <= frfsConfig.straightLineDistance
              )
              serviceableStops

      stopsWithDistance <- tryStationsAPIWithOSRMDistances mId merchantOpCity origin (mkStationsAPI stopsWithinRadius)
      let stopsWithinActualRadius = filter (\stop -> maybe True (\distance -> distance <= frfsConfig.radius) stop.distance) stopsWithDistance

      return
        API.Types.UI.FRFSTicketService.AutocompleteRes
          { routes = [],
            stops = stopsWithinActualRadius
          }
    Just userInput -> do
      let mbLimitInteger = fmap toInteger mbLimit
          mbOffsetInteger = fmap toInteger mbOffset
      matchingStops <- QStation.findAllMatchingStations (Just userInput) mbLimitInteger mbOffsetInteger merchantOpCity.id vehicle
      matchingRoutes <- QRoute.findAllMatchingRoutes (Just userInput) mbLimitInteger mbOffsetInteger merchantOpCity.id vehicle
      currentTime <- getCurrentTime
      let serviceableStops = DTB.findBoundedDomain matchingStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) matchingStops
          serviceableRoutes = DTB.findBoundedDomain matchingRoutes currentTime ++ filter (\route -> route.timeBounds == DTB.Unbounded) matchingRoutes
      stopsWithDistance <- tryStationsAPIWithOSRMDistances mId merchantOpCity origin $ mkStationsAPI (filter (\stop -> maybe True (any (`elem` [END, INTERMEDIATE])) stop.possibleTypes) serviceableStops)

      let routes =
            map
              ( \route ->
                  FRFSRouteAPI
                    { code = route.code,
                      shortName = route.shortName,
                      longName = route.longName,
                      startPoint = route.startPoint,
                      endPoint = route.endPoint,
                      timeBounds = Just route.timeBounds,
                      totalStops = Nothing,
                      stops = Nothing,
                      waypoints = Nothing
                    }
              )
              serviceableRoutes

      return
        API.Types.UI.FRFSTicketService.AutocompleteRes
          { routes = routes,
            stops = stopsWithDistance
          }
  where
    mkStationsAPI =
      map
        ( \Station {..} ->
            FRFSStationAPI
              { color = Nothing,
                distance = Nothing,
                sequenceNum = Nothing,
                stationType = Nothing,
                towards = Nothing,
                ..
              }
        )

tryStationsAPIWithOSRMDistances :: Id Merchant.Merchant -> MerchantOperatingCity -> LatLong -> [FRFSStationAPI] -> Environment.Flow [FRFSStationAPI]
tryStationsAPIWithOSRMDistances merchantId merchantOpCity origin stops = do
  if null stops
    then return []
    else do
      let transformedStops =
            map
              ( \stop ->
                  StationResult
                    { lat = fromMaybe merchantOpCity.lat stop.lat,
                      lon = fromMaybe merchantOpCity.long stop.lon,
                      code = stop.code,
                      name = stop.name,
                      stationType = stop.stationType,
                      sequenceNum = stop.sequenceNum
                    }
              )
              stops

      let maxBatchSize = 100
          stopBatches = chunksOf maxBatchSize transformedStops

      batchedResults <- fmap concat $
        forM stopBatches $ \batch -> do
          res <-
            try @_ @SomeException $
              Maps.getFrfsAutocompleteDistances merchantId merchantOpCity.id $
                GetDistancesReq
                  { origins = NonEmpty.fromList batch,
                    destinations = NonEmpty.fromList [origin],
                    distanceUnit = Meter,
                    sourceDestinationMapping = Nothing,
                    travelMode = Just Maps.CAR
                  }
          case res of
            Left _ -> return $ map (\StationResult {..} -> FRFSStationAPI {lat = Just lat, lon = Just lon, address = Nothing, color = Nothing, distance = Nothing, towards = Nothing, ..}) batch
            Right stopsDistanceResp ->
              return $ map (\stop -> mkStopToAPI stop.origin stop.distance) (NonEmpty.toList stopsDistanceResp)

      return batchedResults
  where
    mkStopToAPI stop distance =
      FRFSStationAPI
        { name = stop.name,
          code = stop.code,
          lat = Just stop.lat,
          lon = Just stop.lon,
          distance = Just distance,
          stationType = stop.stationType,
          sequenceNum = stop.sequenceNum,
          address = Nothing,
          color = Nothing,
          towards = Nothing
        }

postFrfsTicketVerify ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Context.City ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  API.Types.UI.FRFSTicketService.FRFSTicketVerifyReq ->
  Environment.Flow APISuccess.APISuccess
postFrfsTicketVerify (_mbPersonId, merchantId) opCity vehicleCategory req = do
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchantId) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show opCity)
  CallExternalBPP.verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory req.qrData DIBC.APPLICATION
  return APISuccess.Success
