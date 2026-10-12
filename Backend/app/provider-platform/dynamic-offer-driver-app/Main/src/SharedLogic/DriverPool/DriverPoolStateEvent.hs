module SharedLogic.DriverPool.DriverPoolStateEvent
  ( DriverPoolStateEnv,
    DriverPoolStateEvent (..),
    PoolField,
    poolField,
    publishDriverPoolData,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as AT
import qualified Data.Text.Encoding as TE
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Env needed to publish driver pool data events. A 'Nothing' topic
-- disables publishing, so the event can be switched on per deployment.
type DriverPoolStateEnv r = (HasKafkaProducer r, HasField "driverPoolStateTopic" r (Maybe Text))

-- | One changed field of a driver's pool data, named as in 'DriverPoolData'.
type PoolField = AT.Pair

poolField :: ToJSON a => Text -> a -> PoolField
poolField key val = AK.fromText key A..= val

-- | One change to a driver's pool data: only the fields the caller changed.
-- A field cleared to 'Nothing' is sent as null.
data DriverPoolStateEvent = DriverPoolStateEvent
  { driverId :: Text,
    -- | Epoch milliseconds when the event was published.
    ts :: Milliseconds,
    changes :: A.Value
  }
  deriving (Generic, Show, ToJSON)

-- | Publishes the fields a caller changed as their own stream. Independent of
-- 'SharedLogic.DriverPool.LTSDataSync': it reads nothing (no LTS Redis, no DB)
-- and uses none of its types. With no topic configured, or no fields, it sends
-- nothing.
--
-- Best-effort: a failed send is logged and never reaches the caller.
publishDriverPoolData ::
  (MonadFlow m, MonadReader r m, DriverPoolStateEnv r) =>
  Id Driver ->
  [PoolField] ->
  m ()
publishDriverPoolData dId fields = do
  mbTopic <- asks (.driverPoolStateTopic)
  whenJust mbTopic $ \topic -> unless (null fields) $ do
    nowMs <- getCurrentTimestamp -- wall-clock epoch ms; getClockTimeInMs is a since-boot clock
    let event =
          DriverPoolStateEvent
            { driverId = dId.getId,
              ts = Milliseconds (round nowMs),
              changes = A.object fields
            }
    res <- withTryCatch "publishDriverPoolData" $ produceMessage (topic, Just $ TE.encodeUtf8 dId.getId) event
    case res of
      Left err -> logError $ "publishDriverPoolData failed for driver " <> dId.getId <> ": " <> show err
      Right () -> pure ()
