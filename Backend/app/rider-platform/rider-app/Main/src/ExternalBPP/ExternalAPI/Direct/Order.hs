module ExternalBPP.ExternalAPI.Direct.Order where

import API.Types.UI.FRFSTicketService
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Format
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DIRECTConfig -> Id IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> m ProviderOrder
createOrder config integratedBPPConfigId qrTtl booking = do
  when (isJust booking.bppOrderId) $ throwError (InternalError $ "Order Already Created for Booking : " <> booking.id.getId)
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) :: Integer) -- This should be max 20 characters UUID (Using Transaction UUID)
      mbRouteStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
  routeStations <- mbRouteStations & fromMaybeM (InternalError "Route Stations Not Found.")
  tickets <- mapM (getTicketDetail config integratedBPPConfigId qrTtl booking) routeStations
  return ProviderOrder {..}

getTicketDetail :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Id IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> FRFSRouteStationsAPI -> m ProviderTicket
getTicketDetail config integratedBPPConfigId qrTtl booking routeStation = do
  busTypeId <- routeStation.vehicleServiceTier <&> (.providerCode) & fromMaybeM (InternalError "Bus Provider Code Not Found.")
  when (null routeStation.stations) $ throwError (InternalError "Empty Stations")
  let startStation = head routeStation.stations
      endStation = last routeStation.stations
  fromStation <- B.runInReplica $ QStation.findByStationCode startStation.code integratedBPPConfigId >>= fromMaybeM (StationNotFound $ startStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfigId.getId)
  toStation <- B.runInReplica $ QStation.findByStationCode endStation.code integratedBPPConfigId >>= fromMaybeM (StationNotFound $ endStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfigId.getId)
  route <- do
    B.runInReplica $
      QRoute.findByRouteCode routeStation.code integratedBPPConfigId
        >>= fromMaybeM (RouteNotFound routeStation.code)
  fromRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code fromStation.code integratedBPPConfigId >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code integratedBPPConfigId.getId)
  toRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code toStation.code integratedBPPConfigId >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code integratedBPPConfigId.getId)
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getCurrentTime
  ticketNumber <- do
    id <- generateGUID
    uuid <- UU.fromText id & fromMaybeM (InternalError "Not being able to parse into UUID")
    return $ show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Int)
  now <- getCurrentTime
  let ticketDescription = "ROUTE: " <> route.shortName <> " | FROM: " <> fromStation.name <> " | TO: " <> toStation.name
      amount = Money $ round routeStation.priceWithCurrency.amount
      adultQuantity = booking.quantity
      qrValidityIST = addUTCTime (secondsToNominalDiffTime 19800) qrValidity
      qrRefreshAt = config.qrRefreshTtl <&> (\ttl -> addUTCTime (secondsToNominalDiffTime ttl) now)
  qrData <-
    generateQR config $
      TicketPayload
        { transactionId = booking.searchId.getId,
          fromRouteProviderCode = fromRoute.providerCode,
          toRouteProviderCode = toRoute.providerCode,
          adultQuantity = adultQuantity,
          childQuantity = 0,
          vehicleTypeProviderCode = busTypeId,
          expiry = formatUtcTime qrValidityIST,
          ticketNumber,
          ticketAmount = amount,
          refreshAt = qrRefreshAt
        }
  return $
    ProviderTicket
      { ticketNumber,
        description = Just ticketDescription,
        qrData,
        qrStatus = "UNCLAIMED",
        qrValidity,
        qrRefreshAt
      }
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime
