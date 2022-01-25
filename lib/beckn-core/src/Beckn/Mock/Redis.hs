module Beckn.Mock.Redis where

import Beckn.Mock.App
import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Except
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.String.Conversions
import Database.Redis hiding (runRedis)
import qualified Database.Redis as Hedis
import GHC.Records.Extra
import Relude hiding (id, ord)

withRedisConnection :: (Connection -> IO a) -> IO a
withRedisConnection = C.bracket connectRedis disconnect

connectRedis :: IO Connection
connectRedis = checkedConnect defaultConnectInfo

type HasRedisConnection e = HasField "redisConnection" e Connection

type HasRedisPrefix e = HasField "redisPrefix" e Text

type HasRedisEnv e = (HasRedisConnection e, HasRedisPrefix e)

runRedis :: (HasRedisEnv e) => Redis a -> MockM e a
runRedis action = do
  redisCon <- asks (.redisConnection)
  liftIO $ Hedis.runRedis redisCon action

data RedisError = NotFound Text | DecodeError Text
  deriving (Show)

type RedisHash = [(BS.ByteString, BS.ByteString)]

class RedisHashable a where
  toRedisHash :: a -> RedisHash
  fromRedisHash :: RedisHash -> Either Text a

write :: (HasRedisEnv e, RedisHashable a) => Text -> a -> MockM e ()
write id val = do
  let hash = toRedisHash val
  key <- buildKey id
  let redisAction = hmset key hash :: Redis (Either Reply Status)
  _ <- runRedis redisAction
  pure ()

buildKey :: (HasRedisEnv e) => Text -> MockM e BS.ByteString
buildKey id = do
  pref <- cs <$> asks (.redisPrefix)
  pure $ mconcat [pref, ":", cs id]

readHash :: (HasRedisEnv e) => Text -> MockM e (Either RedisError RedisHash)
readHash id = do
  key <- buildKey id
  let redisAction = hgetall key
  first (const $ NotFound id) <$> runRedis redisAction

readHashableEither :: (HasRedisEnv e, RedisHashable a) => Text -> MockM e (Either RedisError a)
readHashableEither id = runExceptT $ do
  hash <- ExceptT $ readHash id
  except $ first DecodeError $ fromRedisHash hash

editHashable :: (HasRedisEnv e, RedisHashable a) => (a -> a) -> Text -> MockM e (Either RedisError ())
editHashable func id = runExceptT $ do
  val <- ExceptT $ readHashableEither id
  lift $ write id $ func val
