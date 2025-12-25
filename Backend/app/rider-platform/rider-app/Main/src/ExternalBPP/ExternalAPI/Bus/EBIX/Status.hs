module ExternalBPP.ExternalAPI.Bus.EBIX.Status where

import Data.Aeson
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as Ticket
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.EBIX.Auth
import ExternalBPP.ExternalAPI.Types
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Tools.Error
import Tools.JSON

data CheckMobTicketsReq = CheckMobTicketsReq
  { transNo :: Text,
    agentId :: Text
  }
  deriving (Generic)

instance FromJSON CheckMobTicketsReq where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON CheckMobTicketsReq where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

data Ticket = Ticket
  { wbId :: Maybe Text,
    vehRtoNo :: Maybe Text
  }
  deriving (Generic)

instance FromJSON Ticket where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON Ticket where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

newtype TicketDetails = TicketDetails
  { ticketDetails :: [Ticket]
  }
  deriving (Generic)

instance FromJSON TicketDetails where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON TicketDetails where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

newtype CheckMobTicketsRes = CheckMobTicketsRes
  { _data :: TicketDetails
  }
  deriving (Generic)

instance FromJSON CheckMobTicketsRes where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase . recursiveStrip}

instance ToJSON CheckMobTicketsRes where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase . recursiveStrip}

type CheckMobTicketsAPI =
  "Api"
    :> "Cons"
    :> "CheckMobTickets"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] CheckMobTicketsReq
    :> Get '[JSON] CheckMobTicketsRes

checkMobTicketsAPI :: Proxy CheckMobTicketsAPI
checkMobTicketsAPI = Proxy

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => EBIXConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  updatedTickets <-
    mapM
      ( \ticket -> do
          if ticket.status == Ticket.ACTIVE
            then do
              token <- getAuthToken config
              ticketStatus <-
                callAPI config.networkHostUrl (ET.client checkMobTicketsAPI (Just token) $ CheckMobTicketsReq ticket.ticketNumber config.agentId) "createQR" checkMobTicketsAPI
                  >>= fromEitherM (ExternalAPICallError (Just "CREATE_QR_API") config.networkHostUrl)
              let qrStatus = mkTicketStatus ticketStatus
              return $
                Just $
                  ProviderTicket
                    { ticketNumber = ticket.ticketNumber,
                      vehicleNumber = (listToMaybe ticketStatus._data.ticketDetails) >>= (.vehRtoNo),
                      qrData = ticket.qrData,
                      qrStatus,
                      qrValidity = ticket.validTill,
                      description = ticket.description,
                      qrRefreshAt = ticket.qrRefreshAt,
                      commencingHours = ticket.commencingHours
                    }
            else pure Nothing
      )
      tickets
  return $ catMaybes updatedTickets
  where
    mkTicketStatus ticket =
      case (listToMaybe ticket._data.ticketDetails) >>= (.wbId) of
        Just _wbId -> "CLAIMED"
        Nothing -> "UNCLAIMED"
