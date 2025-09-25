module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types hiding (ONDC)
import Domain.Types.BecknConfig
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
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney as CRISBookJourney
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
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

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case (integrationBPPConfig.providerName, integrationBPPConfig.providerConfig) of
    (Just name, _) -> name
    (_, CMRL _) -> "Chennai Metro Rail Limited"
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

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> m (Bool, [FRFSUtils.FRFSFare])
getFares riderId merchant merchanOperatingCity integrationBPPConfig fareRouteDetails vehicleCategory serviceTier = do
  let (routeCode, startStopCode, endStopCode) = getRouteCodeAndStartAndStop
  let isFareMandatory =
        case integrationBPPConfig.providerConfig of
          ONDC _ -> False
          _ -> True
  case integrationBPPConfig.providerConfig of
    CMRL config' -> do
      fares <-
        CMRLFareByOriginDest.getFareByOriginDest config' $
          CMRLFareByOriginDest.FareByOriginDestReq
            { origin = startStopCode,
              destination = endStopCode,
              ticketType = "SJT"
            }
      return (isFareMandatory, fares)
    ONDC _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
      return (isFareMandatory, fares)
    EBIX _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
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
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
      return (isFareMandatory, fares)
    CRIS config' -> do
      (viaPoints, changeOver) <- getChangeOverAndViaPoints (NE.toList fareRouteDetails) integrationBPPConfig
      redisResp <- Redis.safeGet (mkRouteFareKey startStopCode endStopCode changeOver)
      case redisResp of
        Just frfsFare -> return (isFareMandatory, frfsFare)
        Nothing -> do
          routeFareReq <- JMU.getRouteFareRequest startStopCode endStopCode changeOver viaPoints riderId
          resp <- try @_ @SomeException $ CRISRouteFare.getRouteFare config' merchanOperatingCity.id routeFareReq
          case resp of
            Left err -> do
              logError $ "Error while calling CRIS API: " <> show err
              return (isFareMandatory, [])
            Right fares -> do
              Redis.setExp (mkRouteFareKey startStopCode endStopCode changeOver) fares 3600 -- 1 hour
              return (isFareMandatory, fares)
  where
    mkRouteFareKey startStopCode endStopCode changeOver = "CRIS:" <> startStopCode <> "-" <> endStopCode <> "-" <> changeOver

    getRouteCodeAndStartAndStop :: (Text, Text, Text)
    getRouteCodeAndStartAndStop = do
      let firstFareRouteDetail = NE.head fareRouteDetails
      let lastFareRouteDetail = NE.last fareRouteDetails
      let routeCode = firstFareRouteDetail.routeCode
      let startStopCode = firstFareRouteDetail.startStopCode
      let endStopCode = lastFareRouteDetail.endStopCode
      (routeCode, startStopCode, endStopCode)

createOrder :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m ProviderOrder
createOrder integrationBPPConfig qrTtl (_mRiderName, mRiderNumber) booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLOrder.createOrder config' integrationBPPConfig booking mRiderNumber
    EBIX config' -> EBIXOrder.createOrder config' integrationBPPConfig qrTtl booking
    DIRECT config' -> DIRECTOrder.createOrder config' integrationBPPConfig qrTtl booking
    CRIS config' -> CRISBookJourney.createOrder config' integrationBPPConfig booking
    _ -> throwError $ InternalError "Unimplemented!"

getBppOrderId :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> FRFSTicketBooking -> m (Maybe Text)
getBppOrderId integratedBPPConfig booking = do
  case integratedBPPConfig.providerConfig of
    CMRL _ -> Just <$> CMRLOrder.getBppOrderId booking
    EBIX _ -> Just <$> EBIXOrder.getBppOrderId booking
    DIRECT _ -> Just <$> DIRECTOrder.getBppOrderId booking
    CRIS _ -> Just <$> CRISBookJourney.getBppOrderId booking
    _ -> return Nothing

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus integrationBPPConfig booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStatus.getTicketStatus config' booking
    EBIX config' -> EBIXStatus.getTicketStatus config' booking
    DIRECT config' -> DIRECTStatus.getTicketStatus config' booking
    CRIS _config' -> return []
    _ -> throwError $ InternalError "Unimplemented!"

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> Text -> m TicketPayload
verifyTicket integrationBPPConfig encryptedQrData = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.verifyTicket config' encryptedQrData
    _ -> throwError $ InternalError "Unimplemented!"

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> TicketPayload -> m Text
generateQR integrationBPPConfig ticketPayload = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DirectUTILS.generateQR config' ticketPayload
    _ -> throwError $ InternalError "Unimplemented!"

generateUpdatedQRTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> Id FRFSTicketBooking -> (TicketPayload -> m TicketPayload) -> m [TicketPayload]
generateUpdatedQRTicket integrationBPPConfig ticketBookingId updateFn = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.generateUpdatedQRTicket config' ticketBookingId updateFn
    _ -> throwError $ InternalError "Unimplemented!"

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    _ -> throwError $ InternalError "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> throwError $ InternalError "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> throwError $ InternalError "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m [CMRLStationList.Station]
getStationList integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStationList.getStationList config'
    _ -> throwError $ InternalError "Unimplemented!"

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"

getChangeOverAndViaPoints :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => [BasicRouteDetail] -> IntegratedBPPConfig -> m (Text, Text)
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
  return (viaPoints, changeOver)

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
