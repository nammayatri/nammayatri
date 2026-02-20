module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Control.Lens ((^?), _head)
import Data.List (nub, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types hiding (ONDC)
import Domain.Types.Beckn.FRFS.OnSearch
import Domain.Types.BecknConfig
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
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.Types as CRISTypes
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QPerson
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

data SubwayFareDetail = SubwayFareDetail
  { viaPoints :: Text,
    changeOver :: Text,
    rawChangeOver :: Text,
    getAllFares :: Bool
  }
  deriving (Show)

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id Person -> Id Merchant -> Id MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe SubwayFareDetail -> m [FRFSUtils.FRFSFare]
getFares riderId merchantId merchantOperatingCityId integrationBPPConfig fareRouteDetails vehicleCategory serviceTier subwayFareDetail = do
  let (routeCode, startStopCode, endStopCode) = getRouteCodeAndStartAndStop
  case integrationBPPConfig.providerConfig of
    CMRL config' -> do
      fares <-
        CMRLFareByOriginDest.getFareByOriginDest integrationBPPConfig config' $
          CMRLFareByOriginDest.FareByOriginDestReq
            { origin = startStopCode,
              destination = endStopCode,
              ticketType = "SJT"
            }
      return fares
    CMRLV2 config' -> do
      now <- getCurrentTime
      let travelDatetime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      fares <-
        CMRLV2GetFare.getFare integrationBPPConfig config' riderId.getId $
          CMRLV2GetFare.GetFareReq
            { operatorNameId = config'.operatorNameId,
              fromStationId = extractStationCode startStopCode,
              toStationId = extractStationCode endStopCode,
              ticketTypeId = config'.ticketTypeId,
              merchantId = config'.merchantId,
              travelDatetime = travelDatetime,
              fareTypeId = config'.fareTypeId
            }
      return fares
    ONDC _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
      return fares
    EBIX _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
      return $
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
    DIRECT _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
      return fares
    CRIS config' -> do
      SubwayFareDetail {viaPoints, changeOver, rawChangeOver, getAllFares} <- subwayFareDetail & fromMaybeM (InternalError "SubwayFareDetail not found")
      fares <- callCRISAPI config' changeOver rawChangeOver viaPoints startStopCode endStopCode getAllFares
      return fares
  where
    callCRISAPI config' changeOver rawChangeOver viaPoints startStopCode endStopCode getAllFares = do
      routeFareReq <- getRouteFareRequest startStopCode endStopCode changeOver rawChangeOver viaPoints riderId (config'.useRouteFareV4 /= Just True)
      resp <- withTryCatch "CRIS:getRouteFare" $ if config'.useRouteFareV4 == Just True then CRISRouteFare.getRouteFare config' merchantOperatingCityId routeFareReq getAllFares else CRISRouteFareV3.getRouteFare config' merchantOperatingCityId routeFareReq getAllFares
      case resp of
        Left err -> do
          logError $ "Error while calling CRIS API: " <> show err
          return []
        Right (fares, _) -> return fares

    getRouteCodeAndStartAndStop :: (Text, Text, Text)
    getRouteCodeAndStartAndStop = do
      let firstFareRouteDetail = NE.head fareRouteDetails
      let lastFareRouteDetail = NE.last fareRouteDetails
      let routeCode = firstFareRouteDetail.routeCode
      let startStopCode = firstFareRouteDetail.startStopCode
      let endStopCode = lastFareRouteDetail.endStopCode
      (routeCode, startStopCode, endStopCode)

getRouteFareRequest :: (CoreMetrics m, MonadFlow m, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => Text -> Text -> Text -> Text -> Text -> Id Person -> Bool -> m CRISTypes.CRISFareRequest
getRouteFareRequest sourceCode destCode changeOver rawChangeOver viaPoints personId useDummy = do
  if useDummy
    then getDummyRouteFareRequest
    else do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbMobileNumber <- mapM decrypt person.mobileNumber
      mbImeiNumber <- mapM decrypt person.imeiNumber
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      return $
        CRISTypes.CRISFareRequest
          { mobileNo = mbMobileNumber,
            imeiNo = fromMaybe "ed409d8d764c04f7" mbImeiNumber,
            appSession = sessionId,
            sourceCode = sourceCode,
            destCode = destCode,
            changeOver = changeOver,
            rawChangeOver = rawChangeOver,
            via = viaPoints
          }
  where
    getDummyRouteFareRequest :: MonadFlow m => m CRISTypes.CRISFareRequest
    getDummyRouteFareRequest = do
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      return $
        CRISTypes.CRISFareRequest
          { mobileNo = Just "1111111111",
            imeiNo = "abcdefgh",
            appSession = sessionId,
            sourceCode = sourceCode,
            destCode = destCode,
            changeOver = changeOver,
            rawChangeOver = rawChangeOver,
            via = viaPoints
          }

extractStationCode :: Text -> Text
extractStationCode code = fromMaybe code $ drop 1 (T.splitOn "|" code) ^? _head

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
        CRIS config -> fold (changeOverIndirectStations config) <> fold (changeOverDirectStations config)
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
          return $ fold (mkStations fromStation toStation stops startStopType endStopType)
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
