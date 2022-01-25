module Common.Redis where

import Beckn.Types.Core.Migration.Context
import Beckn.Utils.Logging
import Common.App
import Common.Environment
import Common.Exceptions
import Common.Utils
import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Except
import Core.OnConfirm
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Database.Redis hiding (runRedis)
import qualified Database.Redis as Hedis
import Relude hiding (id, ord)

withRedisConnection :: (Connection -> IO a) -> IO a
withRedisConnection = C.bracket connectRedis disconnect

connectRedis :: IO Connection
connectRedis = checkedConnect defaultConnectInfo

contextKey, orderKey :: BS.ByteString
contextKey = "context"
orderKey = "order"

{-
redisPrefix :: BS.ByteString
redisPrefix = "mock-public-transport-bpp:"
-}

runRedis :: Redis a -> MockM AppEnv a
runRedis action = do
  redisCon <- asks (.redisConnection)
  liftIO $ Hedis.runRedis redisCon action

type RedisHash = [(BS.ByteString, BS.ByteString)]

write :: Context -> Order -> MockM AppEnv ()
write ctx ord = do
  let contextValue = BSL.toStrict $ encodeJSON ctx
      orderValue = BSL.toStrict $ encodeJSON ord
      orderId = cs ord.id
      hashList =
        [ (contextKey, contextValue),
          (orderKey, orderValue)
        ]
  key <- buildKey orderId
  let redisAction = hmset key hashList :: Redis (Either Reply Status)
  _ <- runRedis redisAction
  mockLog INFO $ "inserted context and order into redis; key = " <> ord.id
  pure ()

buildKey :: Text -> MockM AppEnv BS.ByteString
buildKey id = do
  pref <- cs <$> asks (.redisPrefix)
  pure $ mconcat [pref, ":", cs id]

readHash :: Text -> MockM AppEnv (Either Text RedisHash)
readHash id = do
  key <- buildKey id
  let redisAction = hgetall key
  first (const $ "hash not found: " <> id) <$> runRedis redisAction

readCtxOrder :: RedisHash -> Either Text (Context, Order)
readCtxOrder list = do
  -- Either Text
  ctx <- findAndDecode contextKey list
  ord <- findAndDecode orderKey list
  pure (ctx, ord)

readCtxOrderEither :: Text -> MockM AppEnv (Either Text (Context, Order))
readCtxOrderEither orderId = runExceptT $ do
  hash <- ExceptT $ readHash orderId
  except $ readCtxOrder hash

readCtxOrderWithException :: Text -> MockM AppEnv (Context, Order)
readCtxOrderWithException orderId = do
  hash <-
    readHash orderId >>= fromEitherM OrderNotFound
  fromEitherM (\msg -> OtherError $ "failed to decode value: " <> msg) $
    readCtxOrder hash

editOrder :: (Order -> Order) -> Text -> MockM AppEnv (Either Text ())
editOrder func orderId = runExceptT $ do
  (context, order) <- ExceptT $ readCtxOrderEither orderId
  lift $ write context $ func order
