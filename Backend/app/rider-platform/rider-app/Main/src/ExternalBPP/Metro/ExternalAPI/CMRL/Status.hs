module ExternalBPP.Metro.ExternalAPI.CMRL.Status where

import Data.Aeson
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Metro.ExternalAPI.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
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

newtype Ticket = Ticket
  { wbId :: Maybe Text
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

getTicketStatus :: (CoreMetrics m, MonadFlow m, EncFlow m r, CacheFlow m r) => EBIXConfig -> FRFSTicketBooking -> Text -> m Text
getTicketStatus config _booking txnUUID = do
  token <- getAuthToken config
  ticket <-
    callAPI config.networkHostUrl (ET.client checkMobTicketsAPI (Just token) $ CheckMobTicketsReq txnUUID config.agentId) "createQR" checkMobTicketsAPI
      >>= fromEitherM (ExternalAPICallError (Just "CREATE_QR_API") config.networkHostUrl)
  case (listToMaybe ticket._data.ticketDetails) >>= (.wbId) of
    Just _wbId -> return "CLAIMED"
    Nothing -> return "UNCLAIMED"
