module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
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
import Kernel.Randomizer
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Tools.Error

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case integrationBPPConfig.providerConfig of
    CMRL _ -> "Chennai Metro Rail Limited"
    EBIX _ -> "Kolkata Buses"
    DIRECT _ -> "Direct Multimodal Services"
    Domain.Types.IntegratedBPPConfig.ONDC _ -> "ONDC Services"
    CRIS _ -> "CRIS Subway"

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> Text -> Text -> Text -> Spec.VehicleCategory -> m [FRFSUtils.FRFSFare]
getFares riderId merchant merchanOperatingCity integrationBPPConfig routeCode startStopCode endStopCode vehicleCategory = do
  case integrationBPPConfig.providerConfig of
    CMRL config' ->
      CMRLFareByOriginDest.getFareByOriginDest config' $
        CMRLFareByOriginDest.FareByOriginDestReq
          { route = routeCode,
            origin = startStopCode,
            destination = endStopCode,
            ticketType = "SJT"
          }
    ONDC _ -> FRFSUtils.getFares riderId vehicleCategory integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
    EBIX _ -> FRFSUtils.getFares riderId vehicleCategory integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
    DIRECT _ -> FRFSUtils.getFares riderId vehicleCategory integrationBPPConfig merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
    CRIS config' -> do
      redisResp <- Redis.safeGet mkRouteFareKey
      case redisResp of
        Just frfsFare -> return frfsFare
        Nothing -> do
          sessionId <- getRandomInRange (1, 1000000 :: Int) -- TODO: Fix it later
          intermediateStations <- buildStations routeCode startStopCode endStopCode integrationBPPConfig START END
          let viaStations = T.intercalate "-" $ map (.stationCode) $ filter (\station -> station.stationType == INTERMEDIATE) intermediateStations
          let request =
                CRISRouteFare.CRISFareRequest
                  { mobileNo = Just "1111111111", -- dummy number and imei for all other requests to avoid sdkToken confusion
                    imeiNo = "abcdefgh",
                    appSession = sessionId,
                    sourceCode = startStopCode,
                    changeOver = " ",
                    destCode = endStopCode,
                    via = viaStations
                  }
          resp <- try @_ @SomeException $ CRISRouteFare.getRouteFare config' merchanOperatingCity.id request
          case resp of
            Left err -> do
              logError $ "Error while calling CRIS API: " <> show err
              return []
            Right fares -> do
              Redis.setExp mkRouteFareKey fares 3600 -- 1 hour
              return fares
  where
    mkRouteFareKey = "CRIS:" <> startStopCode <> "-" <> endStopCode

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m ProviderOrder
createOrder integrationBPPConfig qrTtl (_mRiderName, mRiderNumber) booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLOrder.createOrder config' booking mRiderNumber
    EBIX config' -> EBIXOrder.createOrder config' integrationBPPConfig qrTtl booking
    DIRECT config' -> DIRECTOrder.createOrder config' integrationBPPConfig qrTtl booking
    CRIS config' -> CRISBookJourney.createOrder config' booking
    _ -> throwError $ InternalError "Unimplemented!"

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

buildStations :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, HasShortDurationRetryCfg r c) => Text -> Text -> Text -> IntegratedBPPConfig -> StationType -> StationType -> m [DStation] -- to see
buildStations routeCode startStationCode endStationCode integratedBPPConfig startStopType endStopType = do
  fromStation <- OTPRest.findByStationCodeAndIntegratedBPPConfigId startStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound startStationCode)
  toStation <- OTPRest.findByStationCodeAndIntegratedBPPConfigId endStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound endStationCode)
  stops <- QRouteStopMapping.findByRouteCode routeCode integratedBPPConfig.id
  mkStations fromStation toStation stops startStopType endStopType & fromMaybeM (StationsNotFound fromStation.id.getId toStation.id.getId)

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
