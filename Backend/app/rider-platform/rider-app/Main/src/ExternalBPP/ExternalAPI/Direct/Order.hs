module ExternalBPP.ExternalAPI.Direct.Order where

import API.Types.UI.FRFSTicketService
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Data.Time as Time
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import Kernel.Beam.Functions as B
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

createOrder :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => DIRECTConfig -> IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> m ProviderOrder
createOrder config integratedBPPConfig qrTtl booking = do
  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  let mbRouteStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
  routeStations <- mbRouteStations & fromMaybeM (InternalError "Route Stations Not Found.")
  tickets <- mapM (getTicketDetail config integratedBPPConfig qrTtl booking) routeStations
  return ProviderOrder {..}

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) :: Integer) -- This should be max 20 characters UUID (Using Transaction UUID)
  return orderId

getTicketDetail :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => DIRECTConfig -> IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> FRFSRouteStationsAPI -> m ProviderTicket
getTicketDetail config integratedBPPConfig qrTtl booking routeStation = do
  busTypeId <- routeStation.vehicleServiceTier <&> (.providerCode) & fromMaybeM (InternalError "Bus Provider Code Not Found.")
  when (null routeStation.stations) $ throwError (InternalError "Empty Stations")
  let startStation = head routeStation.stations
      endStation = last routeStation.stations
  fromStation <- B.runInReplica $ OTPRest.getStationByGtfsIdAndStopCode startStation.code integratedBPPConfig >>= fromMaybeM (StationNotFound $ startStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  toStation <- B.runInReplica $ OTPRest.getStationByGtfsIdAndStopCode endStation.code integratedBPPConfig >>= fromMaybeM (StationNotFound $ endStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  route <- OTPRest.getRouteByRouteId integratedBPPConfig routeStation.code >>= fromMaybeM (RouteNotFound routeStation.code)
  fromRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode fromStation.code route.code integratedBPPConfig <&> listToMaybe >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code integratedBPPConfig.id.getId)
  toRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode toStation.code route.code integratedBPPConfig <&> listToMaybe >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code integratedBPPConfig.id.getId)
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getCurrentTime
  ticketNumber <- do
    id <- generateGUID
    uuid <- UU.fromText id & fromMaybeM (InternalError "Not being able to parse into UUID")
    return $ show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Int)
  now <- getCurrentTime
  otpCode <-
    Redis.get otpKey
      >>= \case
        Just (optCode :: Int) -> return optCode
        Nothing -> do
          otpCode :: Int <- (generateOTPCode >>= (pure . readMaybe . T.unpack)) >>= fromMaybeM (InternalError "Failed to generate OTP Code")
          let endOfDayDiffInSeconds = nominalDiffTimeToSeconds $ diffUTCTime (UTCTime (utctDay now) (timeOfDayToTime $ Time.TimeOfDay 23 59 59)) now
          Redis.setExp otpKey otpCode endOfDayDiffInSeconds.getSeconds
          return otpCode
  let ticketDescription = "ROUTE: " <> route.shortName <> " | FROM: " <> fromStation.name <> " | TO: " <> toStation.name
      amount = Money $ round routeStation.priceWithCurrency.amount
      adultQuantity = booking.quantity
      childQuantity = fromMaybe 0 booking.childTicketQuantity
      qrValidityIST = addUTCTime (secondsToNominalDiffTime 19800) qrValidity
      qrRefreshAt = config.qrRefreshTtl <&> (\ttl -> addUTCTime (secondsToNominalDiffTime ttl) now)
  qrData <-
    generateQR config $
      TicketPayload
        { fromRouteProviderCode = fromRoute.providerCode,
          toRouteProviderCode = toRoute.providerCode,
          adultQuantity = adultQuantity,
          childQuantity = childQuantity,
          vehicleTypeProviderCode = busTypeId,
          expiry = formatUtcTime qrValidityIST,
          ticketNumber,
          ticketAmount = amount,
          refreshAt = qrRefreshAt,
          otpCode = Just otpCode
        }
  return $
    ProviderTicket
      { ticketNumber,
        vehicleNumber = Nothing,
        description = Just ticketDescription,
        qrData,
        qrStatus = "UNCLAIMED",
        qrValidity,
        qrRefreshAt
      }
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime

    otpKey :: Text
    otpKey = "otpKey"
