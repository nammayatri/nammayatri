module Redis where

import Beckn.Prelude hiding (read)
import Beckn.Types.Core.Migration.Context
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import Core.OnConfirm
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Redis hiding (runRedis)
import qualified Database.Redis as Hedis
import Types.App
import Utils

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

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

write :: Context -> Order -> MockM ()
write ctx ord = do
  let contextValue = BSL.toStrict $ encodeJSON ctx
      orderValue = BSL.toStrict $ encodeJSON ord
      orderId = E.encodeUtf8 ord.id
      hashList =
        [ (contextKey, contextValue),
          (orderKey, orderValue)
        ]
      redisAction = hmset (redisPrefix <> orderId) hashList :: Redis (Either Reply Status)
  _ <- runRedis redisAction
  mockLog INFO $ "inserted context and order into redis; key = " <> ord.id
  pure ()

read :: Text -> MockM (Either Text (Context, Order))
read orderId = do
  let redisAction = hgetall (redisPrefix <> E.encodeUtf8 orderId)
  maybeList <- rightToMaybe <$> runRedis redisAction
  let maybeCtxOrd = do
        -- Either Text
        list <- maybeToEither "order not found" maybeList
        ctxRaw <- maybeToEither "context value not found" $ lookup contextKey list
        ordRaw <- maybeToEither "order value not found" $ lookup orderKey list
        ctx <- maybeToEither "failed to decode context" $ decodeJSON $ BSL.fromStrict ctxRaw
        --        ord <- maybeToEither "failed to decode order" $ decodeJSON $ BSL.fromStrict ordRaw
        ordJSON <- maybeToEither "failed to decode order" $ Ae.decode $ BSL.fromStrict ordRaw
        ord <- first T.pack $ Ae.parseEither parseJSON ordJSON
        pure (ctx, ord)
  pure maybeCtxOrd

editOrder :: (Order -> Order) -> Text -> MockM (Either Text ())
editOrder func orderId =
  read orderId
    >>= either (pure . Left) (\(ctx, ord) -> Right <$> write ctx (func ord))
