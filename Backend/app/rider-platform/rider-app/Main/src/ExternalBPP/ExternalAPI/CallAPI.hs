module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types hiding (ONDC)
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Order as EBIXOrder
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Status as EBIXStatus
import qualified ExternalBPP.ExternalAPI.Direct.Order as DIRECTOrder
import qualified ExternalBPP.ExternalAPI.Direct.Status as DIRECTStatus
import qualified ExternalBPP.ExternalAPI.Direct.Utils as DirectUTILS
import qualified ExternalBPP.ExternalAPI.Direct.Verify as DIRECTVerify
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.BusinessHour as CMRLBusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.DurationDetails as CMRLDurationDetails
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareByOriginDest as CMRLFareByOriginDest
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareMatrix as CMRLFareMatrix
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.Order as CMRLOrder
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.PassengerViewStatus as CMRLPassengerViewStatus
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.TicketStatus as CMRLStatus
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.BusinessHour as CMRLV2BusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare as CMRLV2GetFare
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.Order as CMRLV2Order
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.StationList as CMRLV2StationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketStatus as CMRLV2TicketStatus
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney as CRISBookJourney
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.RouteFareV3 as CRISRouteFareV3
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case (integrationBPPConfig.providerName, integrationBPPConfig.providerConfig) of
    (Just name, _) -> name
    (_, CMRL _) -> "Chennai Metro Rail Limited"
    (_, CMRLV2 _) -> "Chennai Metro Rail Limited v2"
    (_, EBIX _) -> "Kolkata Buses"
    (_, DIRECT _) -> "Direct Multimodal Services"
    (_, ONDC _) -> "ONDC Services"
    (_, CRIS _) -> "CRIS Subway"

data BasicRouteDetail = BasicRouteDetail
  { routeCode :: Text,
    startStopCode :: Text,
    endStopCode :: Text
  }
  deriving (Show)

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe Text -> [Spec.ServiceTierType] -> [DQuote.FRFSQuoteType] -> m (Bool, [FRFSUtils.FRFSFare])
getFares riderId merchant merchanOperatingCity integrationBPPConfig fareRouteDetails vehicleCategory serviceTier mbSearchReqId blacklistedServiceTiers blacklistedFareQuoteTypes = do
  let (routeCode, startStopCode, endStopCode) = getRouteCodeAndStartAndStop
  let isFareMandatory =
        case integrationBPPConfig.providerConfig of
          ONDC _ -> False
          _ -> True
  case integrationBPPConfig.providerConfig of
    CMRL config' -> do
      fares <-
        CMRLFareByOriginDest.getFareByOriginDest integrationBPPConfig config' $
          CMRLFareByOriginDest.FareByOriginDestReq
            { origin = startStopCode,
              destination = endStopCode,
              ticketType = "SJT"
            }
      return (isFareMandatory, fares)
    CMRLV2 config' -> do
      now <- getCurrentTime
      let travelDatetime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      fares <-
        CMRLV2GetFare.getFare integrationBPPConfig config' $
          CMRLV2GetFare.GetFareReq
            { operatorNameId = config'.operatorNameId,
              fromStationId = startStopCode,
              toStationId = endStopCode,
              ticketTypeId = config'.ticketTypeId,
              merchantId = config'.merchantId,
              travelDatetime = travelDatetime,
              fareTypeId = config'.fareTypeId
            }
      return (isFareMandatory, fares)
    ONDC _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode blacklistedServiceTiers blacklistedFareQuoteTypes
      return (isFareMandatory, fares)
    EBIX _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode blacklistedServiceTiers blacklistedFareQuoteTypes
      return
        ( isFareMandatory,
          map
            ( \FRFSUtils.FRFSFare {..} ->
                let FRFSUtils.FRFSVehicleServiceTier {..} = vehicleServiceTier
                 in FRFSUtils.FRFSFare
                      { vehicleServiceTier =
                          FRFSUtils.FRFSVehicleServiceTier
                            { serviceTierType =
                                case serviceTierType of
                                  Spec.ASHOK_LEYLAND_AC -> Spec.AC
                                  Spec.MIDI_AC -> Spec.AC
                                  Spec.VOLVO_AC -> Spec.AC
                                  Spec.ELECTRIC_V -> Spec.AC
                                  Spec.ELECTRIC_V_PMI -> Spec.AC
                                  a -> a,
                              ..
                            },
                        ..
                      }
            )
            fares
        )
    DIRECT _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode blacklistedServiceTiers blacklistedFareQuoteTypes
      return (isFareMandatory, fares)
    CRIS config' -> do
      (viaPoints, changeOver, rawChangeOver) <- getChangeOverAndViaPoints (NE.toList fareRouteDetails) integrationBPPConfig
      maybe
        (callCRISAPI config' changeOver rawChangeOver viaPoints >>= \fares -> return (isFareMandatory, filterFares fares))
        ( \searchReqId -> do
            let redisKey = CRISRouteFare.mkRouteFareKey startStopCode endStopCode searchReqId
            redisResp <- Redis.safeGet redisKey
            case redisResp of
              Just subwayFares -> do
                let cachedFares = filter (\fare -> maybe False (\fd -> fd.via == rawChangeOver) fare.fareDetails) subwayFares
                if null cachedFares
                  then fetchAndCacheFares config' redisKey changeOver rawChangeOver viaPoints isFareMandatory
                  else return (isFareMandatory, filterFares cachedFares)
              Nothing -> fetchAndCacheFares config' redisKey changeOver rawChangeOver viaPoints isFareMandatory
        )
        mbSearchReqId
  where
    filterFares :: [FRFSUtils.FRFSFare] -> [FRFSUtils.FRFSFare]
    filterFares fares =
      filter
        ( \fare ->
            notElem fare.vehicleServiceTier.serviceTierType blacklistedServiceTiers
              && maybe True (\ft -> notElem ft blacklistedFareQuoteTypes) (fare.fareQuoteType)
        )
        fares

    callCRISAPI config' changeOver rawChangeOver viaPoints = do
      let (_, startStop, endStop) = getRouteCodeAndStartAndStop
      routeFareReq <- JMU.getRouteFareRequest startStop endStop changeOver rawChangeOver viaPoints riderId (config'.useRouteFareV4 /= Just True)
      resp <- withTryCatch "CRIS:getRouteFare" $ if config'.useRouteFareV4 == Just True then CRISRouteFare.getRouteFare config' merchanOperatingCity.id routeFareReq False else CRISRouteFareV3.getRouteFare config' merchanOperatingCity.id routeFareReq True False
      case resp of
        Left err -> do
          logError $ "Error while calling CRIS API: " <> show err
          return []
        Right (fares, _) -> return fares

    fetchAndCacheFares config' redisKey changeOver rawChangeOver viaPoints isFareMandatory = do
      fares <- callCRISAPI config' changeOver rawChangeOver viaPoints
      let filteredFares = filterFares fares
      unless (null fares) $ Redis.setExp redisKey fares 1800
      return (isFareMandatory, filteredFares)

    getRouteCodeAndStartAndStop :: (Text, Text, Text)
    getRouteCodeAndStartAndStop = do
      let firstFareRouteDetail = NE.head fareRouteDetails
      let lastFareRouteDetail = NE.last fareRouteDetails
      let routeCode = firstFareRouteDetail.routeCode
      let startStopCode = firstFareRouteDetail.startStopCode
      let endStopCode = lastFareRouteDetail.endStopCode
      (routeCode, startStopCode, endStopCode)

createOrder :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> [FRFSQuoteCategory] -> m ProviderOrder
createOrder integrationBPPConfig qrTtl (_mRiderName, mRiderNumber) booking quoteCategories = do
  Metrics.startMetrics Metrics.CREATE_ORDER_FRFS (getProviderName integrationBPPConfig) booking.searchId.getId booking.merchantOperatingCityId.getId
  resp <-
    case integrationBPPConfig.providerConfig of
      CMRL config' -> CMRLOrder.createOrder config' integrationBPPConfig booking quoteCategories mRiderNumber
      CMRLV2 config' -> CMRLV2Order.createOrder config' integrationBPPConfig booking quoteCategories mRiderNumber
      EBIX config' -> EBIXOrder.createOrder config' integrationBPPConfig qrTtl booking quoteCategories
      DIRECT config' -> DIRECTOrder.createOrder config' integrationBPPConfig qrTtl booking quoteCategories
      CRIS config' -> CRISBookJourney.createOrder config' integrationBPPConfig booking quoteCategories
      _ -> throwError $ InternalError "Unimplemented!"
  Metrics.finishMetrics Metrics.CREATE_ORDER_FRFS (getProviderName integrationBPPConfig) booking.searchId.getId booking.merchantOperatingCityId.getId
  return resp

getBppOrderId :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> FRFSTicketBooking -> m (Maybe Text)
getBppOrderId integratedBPPConfig booking = do
  case integratedBPPConfig.providerConfig of
    CMRL _ -> Just <$> CMRLOrder.getBppOrderId booking
    CMRLV2 _ -> Just <$> CMRLV2Order.getBppOrderId booking
    EBIX _ -> Just <$> EBIXOrder.getBppOrderId booking
    DIRECT _ -> Just <$> DIRECTOrder.getBppOrderId booking
    CRIS _ -> Just <$> CRISBookJourney.getBppOrderId booking
    _ -> return Nothing

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus integrationBPPConfig booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStatus.getTicketStatus config' booking
    CMRLV2 config' -> CMRLV2TicketStatus.getTicketStatus config' booking
    EBIX config' -> EBIXStatus.getTicketStatus config' booking
    DIRECT config' -> DIRECTStatus.getTicketStatus config' booking
    CRIS _config' -> return []
    _ -> throwError $ InternalError "Unimplemented!"

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> Text -> m TicketPayload
verifyTicket integrationBPPConfig encryptedQrData = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.verifyTicket config' encryptedQrData
    _ -> throwError $ InternalError "Unimplemented!"

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> TicketPayload -> m Text
generateQR integrationBPPConfig ticketPayload = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DirectUTILS.generateQR config' ticketPayload
    _ -> throwError $ InternalError "Unimplemented!"

generateUpdatedQRTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> Id FRFSTicketBooking -> (TicketPayload -> m TicketPayload) -> m [TicketPayload]
generateUpdatedQRTicket integrationBPPConfig ticketBookingId updateFn = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.generateUpdatedQRTicket config' ticketBookingId updateFn
    _ -> throwError $ InternalError "Unimplemented!"

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    CMRLV2 config' -> do
      response <- CMRLV2BusinessHour.getBusinessHour config'
      -- Convert V2 response to V1 format for compatibility
      let findParam name = maybe "" (.paramValue) $ find (\p -> p.paramName == name) response.commonParamList
      return
        CMRLBusinessHour.BusinessHourResult
          { qrBookingStartTime = findParam "qrBookingStartTime",
            qrBookingEndTime = findParam "qrBookingEndTime",
            businessStartTime = findParam "businessStartTime",
            businessEndTime = findParam "businessEndTime",
            qrTicketRestrictionStartTime = findParam "qrTicketRestrictionStartTime",
            qrTicketRestrictionEndTime = findParam "qrTicketRestrictionEndTime"
          }
    _ -> throwError $ InternalError "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> throwError $ InternalError "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> throwError $ InternalError "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> m [CMRLStationList.Station]
getStationList integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStationList.getStationList config'
    CMRLV2 config' -> do
      stations <- CMRLV2StationList.getStationList config'
      return $
        map
          ( \s ->
              CMRLStationList.Station
                { id = 0,
                  lineId = "",
                  stationId = s.stationId,
                  code = s.stationCode,
                  name = s.stationName,
                  taName = Nothing,
                  address = "",
                  latitude = fromMaybe 0.0 s.latitude,
                  longitude = fromMaybe 0.0 s.longitude,
                  sequenceNo = 0
                }
          )
          stations
    _ -> throwError $ InternalError "Unimplemented!"

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"

getChangeOverAndViaPoints :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => [BasicRouteDetail] -> IntegratedBPPConfig -> m (Text, Text, Text)
getChangeOverAndViaPoints fareRouteDetails integrationBPPConfig = do
  allStations <- buildStations fareRouteDetails integrationBPPConfig
  let stationCodes = map (.stationCode) allStations
      viaStations = case stationCodes of
        [] -> []
        [_] -> []
        xs -> nub $ drop 1 (take (length xs - 1) xs)
      changeOverStationCodes = nub $ concatMap (\rd -> [rd.startStopCode, rd.endStopCode]) fareRouteDetails
      changeOverPoints = case changeOverStationCodes of
        [] -> []
        [_] -> []
        xs -> nub $ drop 1 (take (length xs - 1) xs)
      configuredChangeOverStations = case integrationBPPConfig.providerConfig of
        CRIS config -> fromMaybe [] (changeOverIndirectStations config) <> fromMaybe [] (changeOverDirectStations config)
        _ -> []
      changeOverStations = filter (`elem` configuredChangeOverStations) changeOverPoints
      viaPoints = if null viaStations then " " else T.intercalate "-" viaStations
      changeOver = if null changeOverStations then " " else T.intercalate "-" changeOverStations
      rawChangeOver = if null changeOverPoints then " " else T.intercalate "-" changeOverPoints
  return (viaPoints, changeOver, rawChangeOver)

buildStations :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => [BasicRouteDetail] -> IntegratedBPPConfig -> m [DStation]
buildStations basicRouteDetails integratedBPPConfig = do
  let lastStopIndex = length basicRouteDetails - 1
  stationsArray <- do
    mapWithIndexM
      ( \idx routeDetail -> do
          let startStopType = if idx == 0 then START else TRANSIT
          let endStopType = if idx == lastStopIndex then END else TRANSIT
          fromStation <- OTPRest.getStationByGtfsIdAndStopCode routeDetail.startStopCode integratedBPPConfig >>= fromMaybeM (StationNotFound routeDetail.startStopCode)
          toStation <- OTPRest.getStationByGtfsIdAndStopCode routeDetail.endStopCode integratedBPPConfig >>= fromMaybeM (StationNotFound routeDetail.endStopCode)
          stops <- OTPRest.getRouteStopMappingByRouteCode routeDetail.routeCode integratedBPPConfig
          return $ fromMaybe [] (mkStations fromStation toStation stops startStopType endStopType)
      )
      basicRouteDetails
  return $ concat stationsArray
  where
    mapWithIndexM f xs = zipWithM f [0 ..] xs

mkStations :: Station -> Station -> [RouteStopMapping] -> StationType -> StationType -> Maybe [DStation]
mkStations fromStation toStation stops startStopType endStopType =
  ((,) <$> find (\stop -> stop.stopCode == fromStation.code) stops <*> find (\stop -> stop.stopCode == toStation.code) stops)
    <&> \(startStop, endStop) ->
      do
        let startStation = DStation startStop.stopCode startStop.stopName (Just startStop.stopPoint.lat) (Just startStop.stopPoint.lon) startStopType (Just startStop.sequenceNum) Nothing
            endStation = DStation endStop.stopCode endStop.stopName (Just endStop.stopPoint.lat) (Just endStop.stopPoint.lon) endStopType (Just endStop.sequenceNum) Nothing
            intermediateStations =
              (sortOn (.sequenceNum) $ filter (\stop -> stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum < endStop.sequenceNum) stops)
                <&> (\stop -> DStation stop.stopCode stop.stopName (Just stop.stopPoint.lat) (Just stop.stopPoint.lon) INTERMEDIATE (Just stop.sequenceNum) Nothing)
        [startStation] ++ intermediateStations ++ [endStation]
