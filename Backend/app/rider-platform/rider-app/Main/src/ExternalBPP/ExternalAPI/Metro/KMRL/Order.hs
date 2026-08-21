module ExternalBPP.ExternalAPI.Metro.KMRL.Order
  ( bookTicket,
    getTicketStatus,
    BookTicketReq (..),
    BookedTicket (..),
    TicketStatusInfo (..),
    transformKochiStatus,
    toKMRLTransactionId,
    kochiRailMetro,
    KMRLResponse (..),
    ticketRefIdEnvelope,
    kmrlPayload,
    requiredPayload,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as T
import Domain.Types.Extra.IntegratedBPPConfig (KMRLConfig (..))
import ExternalBPP.ExternalAPI.Metro.KMRL.Transport (KMRLError (..), callKMRL)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.HTTP.Client as HTTP

data BookTicketReq = BookTicketReq
  { sourceStationId :: Text,
    destinationStationId :: Text,
    metroType :: Int,
    ticketType :: Text,
    travellers :: Int,
    ticketFare :: Double,
    transactionId :: Text
  }
  deriving (Generic, Show, ToJSON)

kochiRailMetro :: Int
kochiRailMetro = 1

toKMRLTransactionId :: Text -> Text -> Text
toKMRLTransactionId bapId transactionId = T.toUpper (prefix <> alnum transactionId)
  where
    alnum = T.filter isAsciiAlphaNum
    isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c
    domain =
      case reverse (T.splitOn "." (T.takeWhile (/= '/') (T.replace "https://" "" (T.replace "http://" "" bapId)))) of
        (_tld : "co" : label : _) -> label
        (_tld : label : _) -> label
        [only] -> only
        [] -> ""
    prefix
      | domain == "triffy" = "TRF" <> T.take 3 bapId
      | otherwise = T.take 4 domain

data BookedTicket = BookedTicket
  { ticketGUID :: Text,
    ticketRefId :: Text,
    ticketNo :: Text,
    ticketFare :: Maybe Double,
    ticketStatus :: Maybe Text,
    secondaryTicketStatus :: Maybe Text,
    ticketType :: Maybe Text,
    ticketTypeDispName :: Maybe Text,
    ticketId :: Maybe Text,
    source :: Maybe Text,
    destination :: Maybe Text,
    journeyDate :: Maybe Text,
    journeyMode :: Maybe Text,
    metroLine :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

bookTicket ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  BookTicketReq ->
  m BookedTicket
bookTicket config manager req = do
  logInfo $ "[KMRL:BookTicket] transactionId: " <> req.transactionId <> ", travellers: " <> show req.travellers
  callKMRL config manager config.bookTicketUrl True (bookEnvelope req)
    >>= requiredPayload "bookTicket"

data TicketStatusInfo = TicketStatusInfo
  { secondaryTicketStatus :: Text,
    ticketStatus :: Maybe Text,
    ticketRefId :: Maybe Text,
    ticketType :: Maybe Text,
    checkInTravellerCount :: Maybe Int,
    checkOutTravellerCount :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

getTicketStatus ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  Text ->
  m TicketStatusInfo
getTicketStatus config manager ticketRefId = do
  logInfo $ "[KMRL:TicketStatus] ticketRefId: " <> ticketRefId
  callKMRL config manager config.ticketStatusUrl True (ticketRefIdEnvelope ticketRefId)
    >>= requiredPayload "getTicketStatus"

transformKochiStatus :: Text -> Maybe Text
transformKochiStatus status = case T.toUpper status of
  "UNUSED" -> Just "UNCLAIMED"
  "USED" -> Just "CLAIMED"
  "CANCELLED" -> Just "CANCELLED"
  "EXPIRED" -> Just "EXPIRED"
  _ -> Nothing

data KMRLResponse a = KMRLResponse
  { code :: Int,
    payload :: Maybe a,
    message :: Maybe Text
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (KMRLResponse a) where
  parseJSON = A.withObject "KMRLResponse" $ \o -> do
    inner <- o A..: "Data" <|> o A..: "data"
    KMRLResponse
      <$> inner A..: "code"
      <*> inner A..:? "data"
      <*> inner A..:? "message"

kmrlPayload :: (MonadFlow m) => Text -> KMRLResponse a -> m (Maybe a)
kmrlPayload operation resp
  | resp.code == 200 = pure resp.payload
  | otherwise = throwError $ KMRLGatewayError resp.code (operation <> ": " <> fromMaybe "no message" resp.message)

requiredPayload :: (MonadFlow m) => Text -> KMRLResponse a -> m a
requiredPayload operation resp =
  kmrlPayload operation resp >>= \case
    Just value -> pure value
    Nothing -> throwError $ KMRLDecodeError (operation <> ": KMRL answered 200 with no data")

ticketRefIdEnvelope :: Text -> A.Value
ticketRefIdEnvelope ticketRefId = envelope (A.object ["ticketRefId" A..= ticketRefId]) (A.object [])

bookEnvelope :: BookTicketReq -> A.Value
bookEnvelope req = envelope (A.toJSON req) A.Null

envelope :: A.Value -> A.Value -> A.Value
envelope payload unused =
  A.object
    [ "Data" A..= payload,
      "Risk" A..= unused,
      "Links" A..= unused,
      "Meta" A..= unused
    ]
