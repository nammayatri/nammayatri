module Lib.JourneyModule.Location where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import Domain.Types.Journey
import Domain.Types.Location
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Prelude (intToNominalDiffTime)
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

locationToLatLng :: Location -> LatLong
locationToLatLng Location {..} = LatLong {..}

makeLocationRedisKey :: Id person -> Text
makeLocationRedisKey driverId = mconcat ["locations", ":", driverId.getId]

-- | Data type for rider location event to be pushed to Kafka
data RiderLocationEvent = RiderLocationEvent
  { journeyId :: Text,
    latitude :: Double,
    longitude :: Double,
    timestamp :: UTCTime,
    busVehicleNo :: Maybe Text
  }
  deriving (Generic, Show)

instance A.ToJSON RiderLocationEvent where
  toJSON = A.genericToJSON A.defaultOptions

-- | Push rider location to Kafka topic
pushRiderLocationToKafka ::
  ( MonadFlow m,
    Log m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Id Journey ->
  ApiTypes.RiderLocationReq ->
  Maybe Text ->
  m ()
pushRiderLocationToKafka journeyId ApiTypes.RiderLocationReq {..} busVehicleNo = do
  let event =
        RiderLocationEvent
          { journeyId = journeyId.getId,
            latitude = latLong.lat,
            longitude = latLong.lon,
            timestamp = currTime,
            busVehicleNo = busVehicleNo
          }
  let topicName = "rider-location-updates"
  let key = journeyId.getId
  fork "Pushing rider location to Kafka" $ do
    produceMessage (topicName, Just (TE.encodeUtf8 key)) event
      `catch` \(e :: SomeException) -> do
        logError $ "Failed to push rider location to Kafka: " <> show e

addPoint ::
  ( HedisFlow m env,
    MonadFlow m,
    Log m,
    HasFlowEnv m env '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Id Journey ->
  ApiTypes.RiderLocationReq ->
  Maybe Text ->
  m ()
addPoint journeyId req busVehicleNo = do
  let key = makeLocationRedisKey journeyId
  lPush key $ NE.singleton req
  lTrim key 0 10 -- always keep last 10 points
  Hedis.expire key 21600 -- 6 hours
  -- Push rider location to Kafka topic
  pushRiderLocationToKafka journeyId req busVehicleNo

clearPoints :: HedisFlow m env => Id Journey -> m ()
clearPoints journeyId = do
  let key = makeLocationRedisKey journeyId
  clearList key

getLastThreePoints :: (HedisFlow m env) => Id Journey -> m [ApiTypes.RiderLocationReq]
getLastThreePoints journeyId = do
  currentTime <- getCurrentTime
  points <- lRange (makeLocationRedisKey journeyId) 0 (-1)
  let thirtySecondsAgo = 30
  return (take 3 $ filter (\ApiTypes.RiderLocationReq {..} -> diffUTCTime currentTime currTime <= intToNominalDiffTime thirtySecondsAgo) points)
