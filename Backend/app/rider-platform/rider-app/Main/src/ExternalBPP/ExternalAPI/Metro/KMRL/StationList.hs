module ExternalBPP.ExternalAPI.Metro.KMRL.StationList
  ( getStationList,
    toCMRLStations,
    Station (..),
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Domain.Types.Extra.IntegratedBPPConfig (KMRLConfig (..))
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import ExternalBPP.ExternalAPI.Metro.KMRL.Transport (KMRLError (..), callKMRL, envelope)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.HTTP.Client as HTTP

data Station = Station
  { stationId :: Text,
    displayName :: Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    distance :: Maybe Double,
    metroLine :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, A.ToJSON, A.FromJSON)

getStationList ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  m [Station]
getStationList config manager = do
  response <- callKMRL config manager config.stationListUrl True (envelope (A.object ["metroType" A..= config.metroType]))
  stations <- unwrapResponse "station list" response
  logInfo $ "[KMRL:StationList] Fetched " <> show (length stations) <> " stations"
  pure stations

toCMRLStations :: [Station] -> [CMRLStationList.Station]
toCMRLStations = zipWith toCMRLStation [1 ..]
  where
    toCMRLStation sequenceNo station =
      CMRLStationList.Station
        { id = 0,
          lineId = kochiLineId,
          stationId = station.stationId,
          code = "",
          name = station.displayName,
          taName = Nothing,
          address = "",
          latitude = fromMaybe 0.0 station.latitude,
          longitude = fromMaybe 0.0 station.longitude,
          sequenceNo = sequenceNo
        }

    kochiLineId = "01"

data Response a = Response
  { responseCode :: Int,
    responsePayload :: Maybe a,
    responseMessage :: Maybe Text
  }

instance (A.FromJSON a) => A.FromJSON (Response a) where
  parseJSON = A.withObject "KMRL response" $ \o -> do
    inner <- o A..: "Data" <|> o A..: "data"
    flip (A.withObject "KMRL response Data") inner $ \d ->
      Response <$> d A..: "code" <*> d A..:? "data" <*> d A..:? "message"

unwrapResponse :: (MonadFlow m) => Text -> Response a -> m a
unwrapResponse operation (Response status mbPayload mbMessage) = do
  unless (status == 200) $
    throwError (KMRLGatewayError status (operation <> ": " <> fromMaybe "no message" mbMessage))
  mbPayload & maybe (throwError (KMRLDecodeError (operation <> ": code 200 with no data"))) pure
