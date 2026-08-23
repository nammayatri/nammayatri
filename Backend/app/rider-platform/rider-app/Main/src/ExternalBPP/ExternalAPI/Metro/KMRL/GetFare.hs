module ExternalBPP.ExternalAPI.Metro.KMRL.GetFare
  ( getFare,
    FareReq (..),
    FareData (..),
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Domain.Types.Extra.IntegratedBPPConfig (KMRLConfig (..))
import ExternalBPP.ExternalAPI.Metro.KMRL.Transport (KMRLError (..), callKMRL, envelope)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.HTTP.Client as HTTP

data FareReq = FareReq
  { travellers :: Int,
    sourceStationId :: Text,
    destinationStationId :: Text,
    metroType :: Int,
    ticketType :: Text
  }
  deriving (Generic, Show, Eq, A.ToJSON, A.FromJSON)

data FareData = FareData
  { ticketFare :: Double,
    ticketType :: Maybe Text,
    sourceStationId :: Maybe Text,
    sourceStationDisplayName :: Maybe Text,
    destinationStationId :: Maybe Text,
    destinationStationDisplayName :: Maybe Text,
    journeyDate :: Maybe Text,
    metroLine :: Maybe Text
  }
  deriving (Generic, Show, Eq, A.ToJSON, A.FromJSON)

getFare ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  FareReq ->
  m FareData
getFare config manager fareReq = do
  logInfo $ "[KMRL:GetFare] Quoting " <> fareReq.sourceStationId <> " -> " <> fareReq.destinationStationId <> " (" <> fareReq.ticketType <> ", " <> show fareReq.travellers <> " travellers)"
  response <- callKMRL config manager config.fareUrl True (envelope fareReq)
  unwrapResponse "fare" response

data Response a = Response
  { responseCode :: Double,
    responsePayload :: Maybe a,
    responseMessage :: Maybe Text
  }

instance (A.FromJSON a) => A.FromJSON (Response a) where
  parseJSON = A.withObject "KMRL response" $ \o -> do
    inner <- o A..: "Data" <|> o A..: "data"
    flip (A.withObject "KMRL response Data") inner $ \d ->
      Response <$> d A..: "code" <*> d A..:? "data" <*> d A..:? "message"

unwrapResponse :: (MonadFlow m) => Text -> Response a -> m a
unwrapResponse operation (Response code mbPayload mbMessage) = do
  let status = round code :: Int
  unless (status == 200) $
    throwError (KMRLGatewayError status (operation <> ": " <> fromMaybe "no message" mbMessage))
  mbPayload & maybe (throwError (KMRLDecodeError (operation <> ": code 200 with no data"))) pure
