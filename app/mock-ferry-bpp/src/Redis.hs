module Redis where

import Beckn.Prelude hiding (read)
import Beckn.Types.Core.Migration.Context
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Except
import Core.OnConfirm
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import Database.Redis hiding (runRedis)
import qualified Database.Redis as Hedis
import Exceptions
import Types.App
import Utils

withRedisConnection :: (Connection -> IO a) -> IO a
withRedisConnection = C.bracket connectRedis disconnect

connectRedis :: IO Connection
connectRedis = checkedConnect defaultConnectInfo

contextKey, orderKey :: BS.ByteString
contextKey = "context"
orderKey = "order"

redisPrefix :: BS.ByteString
redisPrefix = "mock-ferry-bpp:"

runRedis :: Redis a -> MockM a
runRedis action = do
  redisCon <- asks (.redisConnection)
  liftIO $ Hedis.runRedis redisCon action

type RedisHash = [(BS.ByteString, BS.ByteString)]

write :: Context -> Order -> MockM ()
write ctx ord = do
  let contextValue = BSL.toStrict $ encodeJSON ctx
      orderValue = BSL.toStrict $ encodeJSON ord
      orderId = cs ord.id
      hashList =
        [ (contextKey, contextValue),
          (orderKey, orderValue)
        ]
      redisAction = hmset (redisPrefix <> orderId) hashList :: Redis (Either Reply Status)
  _ <- runRedis redisAction
  mockLog INFO $ "inserted context and order into redis; key = " <> ord.id
  pure ()

readHash :: Text -> MockM (Either Text RedisHash)
readHash id = do
  let redisAction = hgetall (redisPrefix <> cs id)
  first (const $ "hash not found: " <> id) <$> runRedis redisAction

readCtxOrder :: RedisHash -> Either Text (Context, Order)
readCtxOrder list = do
  -- Either Text
  ctx <- findAndDecode contextKey list
  ord <- findAndDecode orderKey list
  pure (ctx, ord)

readCtxOrderEither :: Text -> MockM (Either Text (Context, Order))
readCtxOrderEither orderId = runExceptT $ do
  hash <- ExceptT $ readHash orderId
  except $ readCtxOrder hash

readCtxOrderWithException :: Text -> MockM (Context, Order)
readCtxOrderWithException orderId = do
  hash <-
    readHash orderId >>= fromEitherM OrderNotFound
  fromEitherM (\msg -> OtherError $ "failed to decode value: " <> msg) $
    readCtxOrder hash

editOrder :: (Order -> Order) -> Text -> MockM (Either Text ())
editOrder func orderId = runExceptT $ do
  (context, order) <- ExceptT $ readCtxOrderEither orderId
  lift $ write context $ func order
