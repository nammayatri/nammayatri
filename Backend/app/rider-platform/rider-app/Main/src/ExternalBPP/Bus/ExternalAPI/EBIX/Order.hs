module ExternalBPP.Bus.ExternalAPI.EBIX.Order where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Format
import qualified Data.UUID as UU
import Domain.Types.FRFSQuote
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Bus.ExternalAPI.EBIX.Auth
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant hiding (route, throwError)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error
import Tools.JSON

data SaveMobTicketReq = SaveMobTicketReq
  { frRouteStopSrlNo :: Text,
    toRouteStopSrlNo :: Text,
    noAdult :: Text,
    noChild :: Text,
    btypeId :: Text,
    tktAmt :: HighPrecMoney,
    transNo :: Text,
    transDate :: Text,
    agentId :: Text
  }
  deriving (Generic)

instance FromJSON SaveMobTicketReq where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON SaveMobTicketReq where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

data SaveMobTicketRes = SaveMobTicketRes
  { mbTktId :: Integer
  }
  deriving (Generic)

instance FromJSON SaveMobTicketRes where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON SaveMobTicketRes where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

type SaveMobTicketAPI =
  "Api"
    :> "Cons"
    :> "SaveMobTicket"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] SaveMobTicketReq
    :> Get '[JSON] SaveMobTicketRes

saveMobTicketAPI :: Proxy SaveMobTicketAPI
saveMobTicketAPI = Proxy

getOrderId :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => EBIXConfig -> FRFSTicketBooking -> m (Text, Integer)
getOrderId config booking = do
  case booking.bppOrderId of
    Just bppOrderId -> do
      tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
      ticketNumber :: Integer <-
        listToMaybe tickets <&> (.ticketNumber)
          >>= readMaybe . T.unpack & fromMaybeM (InternalError "Ticket Number Not Found.")
      return (bppOrderId, ticketNumber)
    Nothing -> do
      bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
      let bookingUUIDInt :: Integer = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) -- This should be max 20 characters UUID (Using Transaction UUID)
      routeId <-
        maybe (throwError (InternalError "Route id not found.")) pure $
          booking.routeId >>= \case
            Bus routeId -> Just routeId
            _ -> Nothing
      busTypeId <- booking.serviceTierProviderCode & fromMaybeM (InternalError "Bus Provider Code Not Found.")
      fromStation <- B.runInReplica $ QStation.findById booking.fromStationId >>= fromMaybeM (StationNotFound booking.fromStationId.getId)
      toStation <- B.runInReplica $ QStation.findById booking.toStationId >>= fromMaybeM (StationNotFound booking.toStationId.getId)
      route <- B.runInReplica $ QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
      fromRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code fromStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code)
      toRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code toStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code)
      now <- addUTCTime (secondsToNominalDiffTime 19800) <$> getCurrentTime
      let ticketReq =
            SaveMobTicketReq
              { frRouteStopSrlNo = fromRoute.providerCode,
                toRouteStopSrlNo = toRoute.providerCode,
                noAdult = show booking.quantity,
                noChild = "0",
                btypeId = busTypeId,
                tktAmt = booking.price.amount,
                transNo = show bookingUUIDInt,
                transDate = formatUtcTime now,
                agentId = config.agentId
              }
      token <- getAuthToken config
      ticket <-
        callAPI config.networkHostUrl (ET.client saveMobTicketAPI (Just token) ticketReq) "saveMobTicket" saveMobTicketAPI
          >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_SAVE_MOB_TICKET_API") config.networkHostUrl)
      return (show bookingUUIDInt, ticket.mbTktId)
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
