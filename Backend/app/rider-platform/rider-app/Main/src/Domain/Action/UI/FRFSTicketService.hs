{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import BecknV2.FRFS.Enums hiding (END, START)
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified BecknV2.OnDemand.Enums as OnDemandEnums
import Control.Monad.Extra hiding (fromMaybeM)
import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty hiding (map, nub)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.Common as Common
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (all, and, concatMap, find, fromList, id, length, map, readMaybe, toList, whenJust) --  toList, fromList doubtful
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Maps.Types
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude hiding (whenJust)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error (MerchantError (MerchantOperatingCityNotFound))
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common hiding (mkPrice)
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Servant hiding (route, throwError)
import qualified SharedLogic.FRFSUtils as Utils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.BookingCancellationReason as QBCR
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
import Tools.Auth
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import qualified Tools.Payment as Payment

-- getFrfsRoutes :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Context.City -> Spec.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
-- getFrfsRoutes (_personId, mId) city vehicleType_ = do
--   merchantOpCity <- CQMOC.findByMerchantIdAndCity mId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
--   routes <- B.runInReplica $ QRoute.findAllByMerchantOperatingCityAndVehicleType merchantOpCity.id vehicleType_
--   return $
--     map
--       ( \Route.Route {..} -> FRFSTicketService.FRFSRouteAPI {..}
--       )
--       routes

getFrfsRoutes ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Maybe Text -> -- startStationCode
  Maybe Text -> -- endStationCode
  Context.City ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
getFrfsRoutes (_personId, _mId) startStationCode endStationCode _city _vehicleType = do
  case (startStationCode, endStationCode) of
    (Just mbstartStationCode, Just mbendStationCode) -> do
      routesWithStop <-
        B.runInReplica $
          QRouteStopMapping.findByStopCode mbstartStationCode

      let routeCodes = nub $ map (.routeCode) routesWithStop

      routeStops <-
        B.runInReplica $
          QRouteStopMapping.findByRouteCodes routeCodes

      -- Fetch the current time to filter the stops
      currentTime <- getCurrentTime
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime

      let groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops

      let filteredStopRouteCodes =
            nub $
              catMaybes $
                map
                  ( \stops ->
                      let mbStartStop = find (\stop -> Just stop.stopCode == startStationCode) stops
                          possibleEndStop =
                            find
                              ( \stop ->
                                  case mbStartStop of
                                    Just startStop -> stop.stopCode == mbendStationCode && stop.sequenceNum > startStop.sequenceNum
                                    Nothing -> stop.stopCode == mbendStationCode -- Always add Stop Sequence in DB for Correctness of Responses
                              )
                              stops
                       in case possibleEndStop of
                            Just endStop -> Just endStop.routeCode
                            Nothing -> Nothing
                  )
                  groupedStops

      routes <- QRoute.findByRouteCodes filteredStopRouteCodes

      return $
        map
          ( \route ->
              FRFSTicketService.FRFSRouteAPI
                { code = route.code,
                  shortName = route.shortName,
                  longName = route.longName,
                  startPoint = route.startPoint,
                  endPoint = route.endPoint
                }
          )
          routes
    _ -> do
      merchantOpCity <- CQMOC.findByMerchantIdAndCity _mId _city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _mId.getId <> "-city-" <> show _city)
      routes <- B.runInReplica $ QRoute.findAllByMerchantOperatingCityAndVehicleType merchantOpCity.id _vehicleType
      return $
        map
          ( \Route.Route {..} -> FRFSTicketService.FRFSRouteAPI {..}
          )
          routes

-- getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Context.City -> Kernel.Prelude.Maybe Text -> Spec.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
-- getFrfsStations (_personId, mId) mbCity mbRouteCode vehicleType_ = do
--   merchantOpCity <-
--     case mbCity of
--       Nothing -> CQMOC.findById (Id "407c445a-2200-c45f-8d67-6f6dbfa28e73") >>= fromMaybeM (MerchantOperatingCityNotFound "merchantOpCityId-407c445a-2200-c45f-8d67-6f6dbfa28e73")
--       Just city -> CQMOC.findByMerchantIdAndCity mId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
--   case mbRouteCode of
--     Just routeCode' -> do
--       routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode'
--       return $
--         map
--           ( \RouteStopMapping.RouteStopMapping {..} -> do
--               let stationType =
--                     if sequenceNum == 1
--                       then START
--                       else
--                         if sequenceNum < length routeStops
--                           then INTERMEDIATE
--                           else END
--               FRFSStationAPI
--                 { name = stopName,
--                   code = stopCode,
--                   lat = Just stopPoint.lat,
--                   lon = Just stopPoint.lon,
--                   stationType = Just stationType,
--                   sequenceNum = Just sequenceNum,
--                   address = Nothing,
--                   distance = Nothing,
--                   color = Nothing,
--                   ..
--                 }
--           )
--           routeStops
--     Nothing -> do
--       stations <- B.runInReplica $ QStation.getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOpCity.id vehicleType_
--       return $
--         map
--           ( \Station.Station {..} ->
--               FRFSTicketService.FRFSStationAPI
--                 { color = Nothing,
--                   stationType = Nothing,
--                   sequenceNum = Nothing,
--                   distance = Nothing,
--                   ..
--                 }
--           )
--           stations

getFrfsStations ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Context.City ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Text ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations (_personId, mId) mbCity mbRouteCode mbStartStopCode vehicleType_ = do
  merchantOpCity <-
    case mbCity of
      Nothing ->
        CQMOC.findById (Id "407c445a-2200-c45f-8d67-6f6dbfa28e73")
          >>= fromMaybeM (MerchantOperatingCityNotFound "merchantOpCityId-407c445a-2200-c45f-8d67-6f6dbfa28e73")
      Just city ->
        CQMOC.findByMerchantIdAndCity mId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)

  case (vehicleType_, mbRouteCode, mbStartStopCode) of
    -- Case for BUS when both routeCode and startStopCode are provided
    (BUS, Just routeCode', Just startStopCode') -> do
      currentTime <- getCurrentTime
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode'
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime
      -- Exclude stops with the same startStopCode and filter to sequenceNum after startStopCode
      let startSeqNum = fromMaybe 0 ((.sequenceNum) <$> find (\stop -> stop.stopCode == startStopCode') serviceableStops)
      let filteredRouteStops = filter (\stop -> stop.stopCode /= startStopCode' && stop.sequenceNum > startSeqNum) serviceableStops
      return $
        map
          ( \RouteStopMapping.RouteStopMapping {..} ->
              FRFSStationAPI
                { name = stopName,
                  code = stopCode,
                  lat = Just stopPoint.lat,
                  lon = Just stopPoint.lon,
                  stationType = Just (if sequenceNum == 1 then START else if sequenceNum < length filteredRouteStops then INTERMEDIATE else END),
                  sequenceNum = Just sequenceNum,
                  address = Nothing,
                  distance = Nothing,
                  color = Nothing,
                  ..
                }
          )
          filteredRouteStops

    -- Case for BUS when only routeCode is provided
    (BUS, Just routeCode', Nothing) -> do
      currentTime <- getCurrentTime
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode'
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime
      let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
      return $
        map
          ( \RouteStopMapping.RouteStopMapping {..} ->
              FRFSStationAPI
                { name = stopName,
                  code = stopCode,
                  lat = Just stopPoint.lat,
                  lon = Just stopPoint.lon,
                  stationType = Just (if sequenceNum == 1 then START else if sequenceNum < length sortedStops then INTERMEDIATE else END),
                  sequenceNum = Just sequenceNum,
                  address = Nothing,
                  distance = Nothing,
                  color = Nothing,
                  ..
                }
          )
          sortedStops

    -- Case for BUS when only startStopCode is known, then show all stops to get endStopCode and finally ask for routes
    (BUS, Nothing, Just startStopCode) -> do
      currentTime <- getCurrentTime
      routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStopCode
      let routeCodes = nub $ map (.routeCode) routesWithStop
      routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes (toList routeCodes)

      let serviceableStops = DTB.findBoundedDomain routeStops currentTime
      let groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
      let filteredStops =
            concatMap
              ( \stops ->
                  let mbStartStop = find (\stop -> stop.stopCode == startStopCode) (toList stops)
                   in sortBy (compare `on` (.sequenceNum)) $
                        filter
                          ( \stop ->
                              case mbStartStop of
                                Just startStop -> stop.stopCode /= startStopCode && stop.sequenceNum > startStop.sequenceNum
                                Nothing -> stop.stopCode /= startStopCode
                          )
                          (toList stops)
              )
              groupedStops

      return $
        map
          ( \RouteStopMapping.RouteStopMapping {..} ->
              FRFSStationAPI
                { name = stopName,
                  code = stopCode,
                  lat = Just stopPoint.lat,
                  lon = Just stopPoint.lon,
                  stationType = Just (if sequenceNum == 1 then START else if sequenceNum < length filteredStops then INTERMEDIATE else END),
                  sequenceNum = Just sequenceNum,
                  address = Nothing,
                  distance = Nothing,
                  color = Nothing,
                  ..
                }
          )
          filteredStops

    -- Default case for METRO
    _ -> do
      -- Maintain existing logic for METRO
      case mbRouteCode of
        Just routeCode' -> do
          routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCode routeCode'
          return $
            map
              ( \RouteStopMapping.RouteStopMapping {..} ->
                  FRFSStationAPI
                    { name = stopName,
                      code = stopCode,
                      lat = Just stopPoint.lat,
                      lon = Just stopPoint.lon,
                      stationType = Just (if sequenceNum == 1 then START else if sequenceNum < length routeStops then INTERMEDIATE else END),
                      sequenceNum = Just sequenceNum,
                      address = Nothing,
                      distance = Nothing,
                      color = Nothing,
                      ..
                    }
              )
              routeStops
        Nothing -> do
          stations <- B.runInReplica $ QStation.getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOpCity.id vehicleType_
          return $
            map
              ( \Station.Station {..} ->
                  FRFSStationAPI
                    { color = Nothing,
                      stationType = Nothing,
                      sequenceNum = Nothing,
                      distance = Nothing,
                      ..
                    }
              )
              stations

-- if routeStops look like
--   [
--   {routeCode = "1:SHUTTLE-U", stopCode = "18465", sequenceNum = 1},
--   {routeCode = "1:SHUTTLE-U", stopCode = "18024", sequenceNum = 2},
--   {routeCode = "1:S23-U", stopCode = "18754", sequenceNum = 1},
--   {routeCode = "1:S23-U", stopCode = "18302", sequenceNum = 2}
-- ]

-- After applying groupBy, groupedStops would look like:
-- [
--   [
--     {routeCode = "1:SHUTTLE-U", stopCode = "18465", sequenceNum = 1}, x
--     {routeCode = "1:SHUTTLE-U", stopCode = "18024", sequenceNum = 2} x
--     {routeCode = "1:SHUTTLE-U", stopCode = "18024", sequenceNum = 3} -- Start
--     {routeCode = "1:SHUTTLE-U", stopCode = "18024", sequenceNum = 4}
--     {routeCode = "1:SHUTTLE-U", stopCode = "18024", sequenceNum = 5}
--   ],
--   [
--     {routeCode = "1:S23-U", stopCode = "18754", sequenceNum = 1}, x
--     {routeCode = "1:S23-U", stopCode = "18302", sequenceNum = 2} x
--     {routeCode = "1:S23-U", stopCode = "18302", sequenceNum = 3} x
--     {routeCode = "1:S23-U", stopCode = "18024", sequenceNum = 4} -- Start
--   ]
-- ]

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Spec.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (mbPersonId, merchantId) vehicleType_ req =
  postFrfsSearchHandler (mbPersonId, merchantId) vehicleType_ req Nothing Nothing

postFrfsSearchHandler :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Spec.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Maybe (Id DPO.PartnerOrgTransaction) -> Maybe (Id DPO.PartnerOrganization) -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearchHandler (mbPersonId, merchantId) vehicleType_ FRFSSearchAPIReq {..} mbPOrgTxnId mbPOrgId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) >>= fromMaybeM (InternalError "Beckn Config not found")
  fromStation <- QStation.findByStationCode fromStationCode >>= fromMaybeM (InvalidRequest "Invalid from station id")
  toStation <- QStation.findByStationCode toStationCode >>= fromMaybeM (InvalidRequest "Invalid to station id")
  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      routeCode
  merchantOperatingCity <- CQMOC.findById fromStation.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show fromStation.merchantOperatingCityId)
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
            ..
          }
  QFRFSSearch.create searchReq
  CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq
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
        route <- getFRFSRouteAPIRespById quote.routeId
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
              _route = route,
              ..
            }
    )
    quotes

postFrfsQuoteConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId = do
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  (rider, dConfirmRes) <- confirm
  -- handle (errHandler dConfirmRes.booking) $
  --   void $ withShortRetry $ CallBPP.init dConfirmRes.bppSubscriberUrl becknInitReq
  merchantOperatingCity <- Common.getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  route <- getFRFSRouteAPIRespById dConfirmRes.routeId
  now <- getCurrentTime
  when (dConfirmRes.status == DFRFSTicketBooking.NEW && dConfirmRes.validTill > now) $ do
    bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes'
  return $ makeBookingStatusAPI dConfirmRes route stations merchantOperatingCity.city
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc

    confirm = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
      unless (personId == quote.riderId) $ throwError AccessDenied
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ InvalidRequest "Quote expired"
      maybeM (buildAndCreateBooking rider quote) (\booking -> return (rider, booking)) (QFRFSTicketBooking.findByQuoteId quoteId)

    buildAndCreateBooking rider quote@DFRFSQuote.FRFSQuote {..} = do
      uuid <- generateGUID
      now <- getCurrentTime
      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote.merchantId,
                price = modifyPrice quote.price $ \p -> HighPrecMoney $ (p.getHighPrecMoney) * (toRational quote.quantity),
                estimatedPrice = modifyPrice quote.price $ \p -> HighPrecMoney $ (p.getHighPrecMoney) * (toRational quote.quantity),
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
                ..
              }
      QFRFSTicketBooking.create booking
      return (rider, booking)

    makeBookingStatusAPI booking _route stations city =
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
          ..
        }

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

webhookHandlerFRFSTicket :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow ()
webhookHandlerFRFSTicket paymentOrderId merchantId = do
  logDebug $ "frfs ticket order bap webhookc call" <> paymentOrderId.getShortId
  order <- QPaymentOrder.findByShortId paymentOrderId >>= fromMaybeM (PaymentOrderNotFound paymentOrderId.getShortId)
  bookingByOrderId <- QFRFSTicketBookingPayment.findByPaymentOrderId order.id >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
  booking' <- B.runInReplica $ QFRFSTicketBooking.findById bookingByOrderId.frfsTicketBookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  void $ getFrfsBookingStatus (Just booking'.riderId, merchantId) booking'.id

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  booking' <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
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
      let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.CONFIRMING -> do
      if booking.validTill < now
        then do
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING bookingId
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else do
          buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      CallExternalBPP.status merchant.id merchantOperatingCity bapConfig booking
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
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
            else do
              txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
              let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
              let payerVpa = paymentStatusResp.payerVpa
                  updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing
              void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent payerVpa bookingId
              paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
              let paymentObj =
                    Just $
                      FRFSTicketService.FRFSBookingPaymentAPI
                        { status = paymentStatus_,
                          paymentOrder = paymentOrder_
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
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing
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
                  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL)
                  let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                  mRiderNumber <- mapM decrypt person.mobileNumber
                  void $ CallExternalBPP.confirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) updatedBooking
                  buildFRFSTicketBookingStatusAPIRes updatedBooking paymentSuccess
                else do
                  paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
                  txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                  let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                      paymentObj =
                        Just
                          FRFSTicketService.FRFSBookingPaymentAPI
                            { status = paymentStatus_,
                              paymentOrder = paymentOrder_
                            }
                  buildFRFSTicketBookingStatusAPIRes booking paymentObj
    DFRFSTicketBooking.CANCELLED -> do
      updateTotalOrderValueAndSettlementAmount booking bapConfig
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      let mbPaymentObj = paymentBooking <&> \tbp -> FRFSTicketService.FRFSBookingPaymentAPI {status = Utils.mkTBPStatusAPI tbp.status, paymentOrder = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.COUNTER_CANCELLED -> do
      updateTotalOrderValueAndSettlementAmount booking bapConfig
      buildFRFSTicketBookingStatusAPIRes booking Nothing
  where
    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing
          }

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing
          }

    buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCityId booking = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
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
                metadataGatewayReferenceId = Nothing --- assigned in shared kernel
              }
      DPayment.createOrderService commonMerchantId commonPersonId createOrderReq (createOrderCall merchantOperatingCityId booking)

    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking

    createOrderCall merchantOperatingCityId booking = Payment.createOrder merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
    orderStatusCall merchantOperatingCityId booking = Payment.orderStatus merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
    commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId_

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL =
      let validTill' = mTTL & fromMaybe validTill
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', ..}
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

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, _) = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId Nothing Nothing personId
  mapM (`buildFRFSTicketBookingStatusAPIRes` Nothing) bookings

buildFRFSTicketBookingStatusAPIRes :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe FRFSTicketService.FRFSBookingPaymentAPI -> Environment.Flow FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking payment = do
  route <- getFRFSRouteAPIRespById booking.routeId
  stations <- mapM (Utils.mkPOrgStationAPI booking.partnerOrgId) =<< (decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db"))
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
        _route = route,
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
makeTicketBookingPaymentAPIStatus CANCELLED = FRFSTicketService.FAILURE

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
    CQFRFSConfig.findByMerchantOperatingCityId ticketBooking.merchantOperatingCityId
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  unless (ticketBooking.status == DFRFSTicketBooking.CONFIRMED) $ throwError (InvalidRequest "Cancellation during incorrect status")
  -- tickets <- QFRFSTicket.findAllByTicketBookingId ticketBooking.id
  -- unless (all (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets) $ throwError (InvalidRequest "Cancellation during incorrect status")
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.SOFT_CANCEL ticketBooking
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
    CQFRFSConfig.findByMerchantOperatingCityId ticketBooking.merchantOperatingCityId
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL ticketBooking
  return APISuccess.Success

getFRFSRouteAPIRespById :: Maybe (Id Route.Route) -> Environment.Flow (Maybe FRFSRouteAPI)
getFRFSRouteAPIRespById =
  maybe
    (pure Nothing)
    ( \routeId -> do
        Route.Route {..} <- QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
        return $ Just FRFSRouteAPI {..}
    )

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
  Domain.Types.FRFSConfig.FRFSConfig {..} <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityId merchantOpCity.id >>= fromMaybeM (InvalidRequest "FRFS Config not found")
  stats <- maybe (pure Nothing) CQP.findPersonStatsById pId
  let isEventOngoing' = fromMaybe False isEventOngoing
      ticketsBookedInEvent = fromMaybe 0 ((.ticketsBookedInEvent) =<< stats)
  return FRFSTicketService.FRFSConfigAPIRes {isEventOngoing = isEventOngoing', ..}

data StationResult = StationResult
  { code :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    vehicleType :: Spec.VehicleCategory,
    lat :: Double,
    lon :: Double
  }

instance HasCoordinates StationResult where
  getCoordinates stop = LatLong (stop.lat) (stop.lon)

-- TODO :: Filter the Stops which are always the END stop for all the routes as it can never be a possible START or INTERMEDIATE stop.
getFrfsAutocomplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Text ->
  Context.City ->
  Kernel.External.Maps.Types.LatLong ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.AutocompleteRes]
getFrfsAutocomplete (_, mId) text opCity origin vehicle = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity mId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> " ,city: " <> show opCity)
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityId merchantOpCity.id
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show merchantOpCity.id)
  case text of
    Nothing -> do
      allStops <- QStation.getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOpCity.id vehicle

      currentTime <- getCurrentTime

      let serviceableStops = DTB.findBoundedDomain allStops currentTime

      let filteredStops =
            filter
              ( \stop ->
                  let straightLineDist = highPrecMetersToMeters (CD.distanceBetweenInMeters origin (LatLong (fromMaybe merchantOpCity.lat stop.lat) (fromMaybe merchantOpCity.long stop.lon)))
                   in straightLineDist <= frfsConfig.straightLineDistance
              )
              serviceableStops

      let transformedStops =
            map
              ( \stop ->
                  StationResult
                    { lat = fromMaybe merchantOpCity.lat stop.lat,
                      lon = fromMaybe merchantOpCity.long stop.lon,
                      code = stop.code,
                      name = stop.name,
                      vehicleType = stop.vehicleType
                    }
              )
              filteredStops

      let stopsReq =
            GetDistancesReq
              { origins = NonEmpty.fromList [origin],
                destinations = NonEmpty.fromList transformedStops,
                distanceUnit = Meter,
                sourceDestinationMapping = Just Maps.ManyToMany,
                travelMode = Just CAR
              }

      stopsDistanceResp <- Maps.getFrfsAutocompleteDistances mId merchantOpCity.id stopsReq

      let finalStops = filter (\stopResp -> stopResp.distance <= (frfsConfig.radius)) (NonEmpty.toList stopsDistanceResp)

      let formattedStops =
            map
              ( \stopResp ->
                  Stop
                    { stopCode = stopResp.destination.code,
                      stopName = stopResp.destination.name,
                      distance = stopResp.distance
                    }
              )
              finalStops

      return
        [ API.Types.UI.FRFSTicketService.AutocompleteRes
            { routes = [],
              stops = formattedStops
            }
        ]
    Just userInput -> do
      allStops <- QStation.getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOpCity.id vehicle
      allRoutes <- QRoute.findAllByMerchantOperatingCityAndVehicleType merchantOpCity.id vehicle
      let input = T.toUpper userInput
          matchingStops = filter (\stop -> T.isInfixOf input (T.toUpper stop.name)) allStops
          matchingRoutes = filter (\route -> T.isInfixOf input (T.toUpper route.longName)) allRoutes

      currentTime <- getCurrentTime

      let serviceableStops = DTB.findBoundedDomain matchingStops currentTime
          serviceableRoutes = DTB.findBoundedDomain matchingRoutes currentTime

      let transformedStops =
            map
              ( \stop ->
                  StationResult
                    { lat = fromMaybe merchantOpCity.lat stop.lat,
                      lon = fromMaybe merchantOpCity.long stop.lon,
                      code = stop.code,
                      name = stop.name,
                      vehicleType = stop.vehicleType
                    }
              )
              serviceableStops

      let stopsReq =
            GetDistancesReq
              { origins = NonEmpty.fromList [origin],
                destinations = NonEmpty.fromList transformedStops,
                distanceUnit = Meter,
                sourceDestinationMapping = Just Maps.ManyToMany,
                travelMode = Just Maps.CAR
              }

      stopsDistanceResp <- Maps.getFrfsAutocompleteDistances mId merchantOpCity.id stopsReq

      let formattedStops =
            map
              ( \stopResp ->
                  Stop
                    { stopCode = stopResp.destination.code,
                      stopName = stopResp.destination.name,
                      distance = stopResp.distance
                    }
              )
              (NonEmpty.toList stopsDistanceResp)

      let formattedRoutes =
            map
              ( \routeResp ->
                  FRFSRouteAPI
                    { code = routeResp.code,
                      shortName = routeResp.shortName,
                      longName = routeResp.longName,
                      startPoint = routeResp.startPoint,
                      endPoint = routeResp.endPoint
                    }
              )
              serviceableRoutes

      return
        [ API.Types.UI.FRFSTicketService.AutocompleteRes
            { routes = formattedRoutes,
              stops = formattedStops
            }
        ]
